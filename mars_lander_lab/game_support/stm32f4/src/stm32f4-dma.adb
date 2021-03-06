------------------------------------------------------------------------------
--                                                                          --
--                Hardware Abstraction Layer for STM32 Targets              --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System.Storage_Elements;

package body STM32F4.DMA is

   ------------
   -- Enable --
   ------------

   procedure Enable
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
      Temp : Stream_Config_Register;
      --  this register requires 32-bit accesses, hence the temporary
   begin
      Temp := Unit.Streams (Stream).CR;
      Temp.Stream_Enabled := True;
      Unit.Streams (Stream).CR := Temp;
   end Enable;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
   is
      Temp : Stream_Config_Register;
      --  this register requires 32-bit accesses, hence the temporary
   begin
      Temp := Unit.Streams (Stream).CR;
      return Temp.Stream_Enabled;
   end Enabled;

   -------------
   -- Disable --
   -------------

   procedure Disable
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
      Temp : Stream_Config_Register;
      --  this register requires 32-bit accesses, hence the temporary
   begin
      Temp := Unit.Streams (Stream).CR;
      Temp.Stream_Enabled := False;
      Unit.Streams (Stream).CR := Temp;
      -- the STMicro Reference Manual RM0090, Doc Id 018909 Rev 6, pg 319, step
      -- 1 says we must await the bit actually clearing, to confirm no ongoing
      -- operation remains active
      loop
         Temp := Unit.Streams (Stream).CR;
         exit when not Temp.Stream_Enabled;
      end loop;
   end Disable;

   ---------------------------
   -- Set_Interrupt_Enabler --
   ---------------------------

   procedure Set_Interrupt_Enabler
     (This_Stream : in out DMA_Stream;
      Source      : DMA_Interrupt;
      Value       : Boolean)
   is
   begin
      if Source = FIFO_Error_Interrupt then -- use the FCR
         declare
            Temp : FIFO_Control_Register;
         begin
            Temp := This_Stream.FCR;
            Temp.FIFO_Interrupt_Enabled := Value;
            This_Stream.FCR := Temp;
         end;
      else -- use the CR
         declare
            Temp : Stream_Config_Register;
         begin
            Temp := This_Stream.CR;
            case Source is
               when Direct_Mode_Error_Interrupt =>
                  Temp.DMEI_Enabled := Value;
               when Transfer_Error_Interrupt =>
                  Temp.TEI_Enabled := Value;
               when Half_Transfer_Complete_Interrupt =>
                  Temp.HTI_Enabled := Value;
               when Transfer_Complete_Interrupt =>
                  Temp.TCI_Enabled := Value;
               when FIFO_Error_Interrupt =>
                  -- not possible since we've already checked for it above
                  null;
            end case;
            This_Stream.CR := Temp;
         end;
      end if;
   end Set_Interrupt_Enabler;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
   is
   begin
      Set_Interrupt_Enabler (Unit.Streams (Stream), Source, True);
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector;
      Source : DMA_Interrupt)
   is
   begin
      Set_Interrupt_Enabler (Unit.Streams (Stream), Source, False);
   end Disable_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (Unit        : DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : DMA_Interrupt)
      return Boolean
   is
      Result      : Boolean;
      This_Stream : DMA_Stream renames Unit.Streams (Stream);
      --  this is a bit heavy, considering it will be called from interrupt
      --  handlers.
      --  TODO: consider a much lower level implementation, based on bit-masks.
   begin
      if Source = FIFO_Error_Interrupt then -- use the FCR
         declare
            Temp : FIFO_Control_Register;
         begin
            Temp := This_Stream.FCR;
            Result := Temp.FIFO_Interrupt_Enabled;
         end;
      else -- use the CR
         declare
            Temp : Stream_Config_Register;
         begin
            Temp := This_Stream.CR;
            case Source is
               when Direct_Mode_Error_Interrupt =>
                  Result := Temp.DMEI_Enabled;
               when Transfer_Error_Interrupt =>
                  Result := Temp.TEI_Enabled;
               when Half_Transfer_Complete_Interrupt =>
                  Result := Temp.HTI_Enabled;
               when Transfer_Complete_Interrupt =>
                  Result := Temp.TCI_Enabled;
               when FIFO_Error_Interrupt =>
                  -- not possible since we've already checked for it above
                  null;
            end case;
         end;
      end if;
      return Result;
   end Interrupt_Enabled;

   --------------------
   -- Start_Transfer --
   --------------------

   procedure Start_Transfer
     (Unit        : in out DMA_Controller;
      Stream      : DMA_Stream_Selector;
      Source      : Address;
      Destination : Address;
      Data_Count  : Half_Word)
   is
   begin
      Disable (Unit, Stream);  --  per the RM, eg section 10.5.6 for the NDTR

      Configure_Data_Flow
        (Unit,
         Stream,
         Source      => Source,
         Destination => Destination,
         Data_Count => Data_Count);

      Enable (Unit, Stream);
   end Start_Transfer;

   ------------------------------------
   -- Start_Transfer_with_Interrupts --
   ------------------------------------

   procedure Start_Transfer_With_Interrupts
     (Unit               : in out DMA_Controller;
      Stream             : DMA_Stream_Selector;
      Source             : Address;
      Destination        : Address;
      Data_Count         : Half_Word;
      Enabled_Interrupts : Interrupt_Selections := (others => True))
   is
   begin
      Disable (Unit, Stream);  --  per the RM, eg section 10.5.6 for the NDTR

      Configure_Data_Flow
        (Unit,
         Stream,
         Source      => Source,
         Destination => Destination,
         Data_Count => Data_Count);

      for Selected_Interrupt in Enabled_Interrupts'Range loop
         if Enabled_Interrupts (Selected_Interrupt) then
            Enable_Interrupt (Unit, Stream, Selected_Interrupt);
         end if;
      end loop;

      Enable (Unit, Stream);
   end Start_Transfer_with_Interrupts;

   -------------------------
   -- Configure_Data_Flow --
   -------------------------

   procedure Configure_Data_Flow
     (Unit              : in out DMA_Controller;
      Stream            : DMA_Stream_Selector;
      Source            : Address;
      Destination       : Address;
      Data_Count : Half_Word)
   is
      This_Stream : DMA_Stream renames Unit.Streams (Stream);
      Temp        : Stream_Config_Register;
   begin
      This_Stream.NDTR := Word (Data_Count);

      Temp := This_Stream.CR;
      if Temp.Direction = Memory_To_Peripheral then
         This_Stream.PAR := Destination;
         This_Stream.M0AR := Source;
      else
         This_Stream.PAR := Source;
         This_Stream.M0AR := Destination;
      end if;
   end Configure_Data_Flow;

   --------------------
   -- Abort_Transfer --
   --------------------

   procedure Abort_Transfer
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector;
      Result : out DMA_Error_Code)
   is
      Max_Abort_Time : constant Time_Span := Seconds (1);
      Timeout        : Time;
      This_Stream    : DMA_Stream renames Unit.Streams (Stream);
      Temp           : Stream_Config_Register;
   begin
      Disable (Unit, Stream);
      Timeout := Clock + Max_Abort_Time;
      loop
         Temp := This_Stream.CR;  -- we need 32-bit accesses
         exit when not Temp.Stream_Enabled;
         if Clock > Timeout then
            Result := DMA_Timeout_Error;
            return;
         end if;
      end loop;
      Result := DMA_No_Error;
   end Abort_Transfer;

   -------------------------
   -- Poll_For_Completion --
   -------------------------

   procedure Poll_For_Completion
     (Unit           : in out DMA_Controller;
      Stream         : DMA_Stream_Selector;
      Expected_Level : DMA_Transfer_Level;
      Timeout        : Time_Span;
      Result         : out DMA_Error_Code)
   is
      Deadline : constant Time := Clock + Timeout;
   begin
      Result := DMA_No_Error;  -- initially anyway

      Polling : loop
         if Expected_Level = Full_Transfer then
            exit when Status (Unit, Stream, Transfer_Complete_Indicated);
         else
            exit when Status (Unit, Stream, Half_Transfer_Complete_Indicated);
         end if;

         if Status (Unit, Stream, Transfer_Error_Indicated) or
            Status (Unit, Stream, FIFO_Error_Indicated) or
            Status (Unit, Stream, Direct_Mode_Error_Indicated)
         then
            Clear_Status (Unit, Stream, Transfer_Error_Indicated);
            Clear_Status (Unit, Stream, FIFO_Error_Indicated);
            Clear_Status (Unit, Stream, Direct_Mode_Error_Indicated);

            Result := DMA_Device_Error;
            return;
         end if;

         if Clock > Deadline then
            Result := DMA_Timeout_Error;
            return;
         end if;
      end loop Polling;

      Clear_Status (Unit, Stream, Half_Transfer_Complete_Indicated);

      if Expected_Level = Full_Transfer then
         Clear_Status (Unit, Stream, Transfer_Complete_Indicated);
      else
         Clear_Status (Unit, Stream, Half_Transfer_Complete_Indicated);
      end if;
   end Poll_For_Completion;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector;
      Flag   : DMA_Status_Flag)
   is
      Group : constant Stream_Group := DMA_Stream_Selector'Pos (Stream) mod 4;
      Bit   : constant Bit_Numbers := Status_Flag_Bits (Flag) (Group);
      Mask  : constant Word := Shift_Left (1, Integer (Bit));
      Temp  : Word;
   begin
      if Stream < Stream_4 then
         Temp := Unit.LIFCR;
         Temp := Temp or Mask;  --  yes, 1, because this is the CLEAR register
         Unit.LIFCR := Temp;
      else
         Temp := Unit.HIFCR;
         Temp := Temp or Mask;  --  yes, 1, because this is the CLEAR register
         Unit.HIFCR := Temp;
      end if;
   end Clear_Status;

   ----------------------
   -- Clear_All_Status --
   ----------------------

   procedure Clear_All_Status
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
      Group : constant Stream_Group := DMA_Stream_Selector'Pos (Stream) mod 4;
      Temp  : Word;
   begin
      if Stream < Stream_4 then
         Temp := Unit.LIFCR;
      else
         Temp := Unit.HIFCR;
      end if;
      for Flag in DMA_Status_Flag loop
         declare
            Bit  : constant Bit_Numbers := Status_Flag_Bits (Flag) (Group);
            Mask : constant Word := Shift_Left (1, Integer (Bit));
         begin
            Temp := Temp or Mask;
         end;
      end loop;
      if Stream < Stream_4 then
         Unit.LIFCR := Temp;
      else
         Unit.HIFCR := Temp;
      end if;
   end Clear_All_Status;

   ------------
   -- Status --
   ------------

   function Status
     (Unit    : DMA_Controller;
      Stream  : DMA_Stream_Selector;
      Flag    : DMA_Status_Flag)
      return Boolean
   is
      Group : constant Stream_Group := DMA_Stream_Selector'Pos (Stream) mod 4;
      Bit   : constant Bit_Numbers := Status_Flag_Bits (Flag) (Group);
      Mask  : constant Word := Shift_Left (1, Integer (Bit));
      Temp  : Word;
   begin
      if Stream < Stream_4 then
         Temp := Unit.LISR;
      else
         Temp := Unit.HISR;
      end if;
      return (Temp and Mask) = Mask;
   end Status;

   -----------------
   -- Set_Counter --
   -----------------

   procedure Set_Counter
     (Unit       : in out DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Data_Count : Half_Word)
   is
      This_Stream : DMA_Stream renames Unit.Streams (Stream);
   begin
      This_Stream.NDTR := Word (Data_Count);
   end Set_Counter;

   ---------------------
   -- Current_Counter --
   ---------------------

   function Current_Counter
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Half_Word
   is
      This_Stream : DMA_Stream renames Unit.Streams (Stream);
   begin
      return Half_Word (This_Stream.NDTR);
   end Current_Counter;

   ---------------------
   -- Double_Buffered --
   ---------------------

   function Double_Buffered
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
   is
      CR : constant Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      return CR.Double_Buffered;
   end Double_Buffered;

   -------------------
   -- Circular_Mode --
   -------------------

   function Circular_Mode
     (Unit   : DMA_Controller;
      Stream : DMA_Stream_Selector)
      return Boolean
   is
      CR : constant Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      return CR.Circular_Mode;
   end Circular_Mode;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector;
      Config : DMA_Stream_Configuration)
   is
      --  see HAL_DMA_Init in STM32F4xx_HAL_Driver\Inc\stm32f4xx_hal_dma.h
      This_Stream : DMA_Stream renames Unit.Streams (Stream);
   begin
      -- the STMicro Reference Manual RM0090, Doc Id 018909 Rev 6, pg 319 says
      -- we must disable the stream before configuring it
      Disable (Unit, Stream);

      declare
         Temp : Stream_Config_Register := This_Stream.CR;
      begin
         Temp.Current_Target := Memory_Buffer_0;

         Temp.Channel := Config.Channel;
         Temp.Direction := Config.Direction;
         Temp.P_Inc_Mode := Config.Increment_Peripheral_Address;
         Temp.M_Inc_Mode := Config.Increment_Memory_Address;
         Temp.P_Data_Size := Config.Peripheral_Data_Format;
         Temp.M_Data_Size := Config.Memory_Data_Format;
         Temp.Priority := Config.Priority;

         case Config.Operation_Mode is
            when Normal_Mode =>
               Temp.Circular_Mode := False;
               Temp.P_Flow_Controller := False;
            when Peripheral_Flow_Control_Mode =>
               Temp.Circular_Mode := False;
               Temp.P_Flow_Controller := True;
            when Circular_Mode =>
               Temp.Circular_Mode := True;
               Temp.P_Flow_Controller := False;
         end case;

         --  the memory burst and peripheral burst values are only used when
         --  the FIFO is enabled
         if Config.FIFO_Enabled then
            Temp.M_Burst := Config.Memory_Burst_Size;
            Temp.P_Burst := Config.Peripheral_Burst_Size;
         else
            Temp.M_Burst := Memory_Burst_Single;
            Temp.P_Burst := Peripheral_Burst_Single;
         end if;

         This_Stream.CR := Temp;
      end;

      declare
         Temp : FIFO_Control_Register := This_Stream.FCR;
      begin
         Temp.Direct_Mode_Enabled := not Config.FIFO_Enabled;

         if Config.FIFO_Enabled then
            Temp.FIFO_Threshold := Config.FIFO_Threshold;
         else
            Temp.FIFO_Threshold := FIFO_Threshold_1_Quart_Full_Configuration; -- 0, default
         end if;
         This_Stream.FCR := Temp;
      end;
   end Configure;

   -----------
   -- Reset --
   -----------

   procedure Reset (Unit : in out DMA_Controller;  Stream : DMA_Stream_Selector) is
      This_Stream : DMA_Stream renames Unit.Streams (Stream);

      function As_Stream_Config_Register is new Ada.Unchecked_Conversion
        (Source => Word, Target => Stream_Config_Register);

      function As_FIFO_Control_Register is new Ada.Unchecked_Conversion
        (Source => Word, Target => FIFO_Control_Register);
   begin
      Disable (Unit, Stream);

      This_Stream.CR := As_Stream_Config_Register (0);
      This_Stream.NDTR := 0;
      This_Stream.PAR := System.Null_Address;
      This_Stream.M0AR := System.Null_Address;
      This_Stream.M1AR := System.Null_Address;

      --  Clear the FIFO control register bits except sets bit 5 to show FIFO
      --  is empty and bit 0 to set threshold selection to 1/2 full (???)
      This_Stream.FCR := As_FIFO_Control_Register (2#100001#);

      Clear_All_Status (Unit, Stream);
   end Reset;

   ---------------------------
   -- Peripheral_Data_Width --
   ---------------------------

   function Peripheral_Data_Width
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Widths
   is
      Current : constant Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      return Current.P_Data_Size;
   end Peripheral_Data_Width;

   -----------------------
   -- Memory_Data_Width --
   -----------------------

   function Memory_Data_Width
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Widths
   is
      Current : constant Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      return Current.M_Data_Size;
   end Memory_Data_Width;

   ------------------------
   -- Transfer_Direction --
   ------------------------

   function Transfer_Direction
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Data_Transfer_Direction
   is
      Current : constant Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      return Current.Direction;
   end Transfer_Direction;

   --------------------
   -- Operating_Mode --
   --------------------

   function Operating_Mode
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Mode
   is
      Current : constant Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      if Current.P_Flow_Controller then
         return Peripheral_Flow_Control_Mode;
      elsif Current.Circular_Mode then
         return Circular_Mode;
      end if;
      return Normal_Mode;
   end Operating_Mode;

   --------------
   -- Priority --
   --------------

   function Priority
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Priority_Level
   is
      Current : constant Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      return Current.Priority;
   end Priority;

   ---------------------------
   -- Current_Memory_Buffer --
   ---------------------------

   function Current_Memory_Buffer
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return Memory_Buffer_Target
   is
      CR : constant Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      return CR.Current_Target;
   end Current_Memory_Buffer;

   -----------------------
   -- Set_Memory_Buffer --
   -----------------------

   procedure Set_Memory_Buffer
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector;
      Buffer : Memory_Buffer_Target;
      To     : System.Address)
   is
      This_Stream : DMA_Stream renames Unit.Streams (Stream);
   begin
      case Buffer is
         when Memory_Buffer_0 =>
            This_Stream.M0AR := To;
         when Memory_Buffer_1 =>
            This_Stream.M1AR := To;
      end case;
   end Set_Memory_Buffer;

   ----------------------------------
   -- Select_Current_Memory_Buffer --
   ----------------------------------

   procedure Select_Current_Memory_Buffer
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector;
      Buffer : Memory_Buffer_Target)
   is
      Temp_CR : Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      Temp_CR.Current_Target := Buffer;
      Unit.Streams (Stream).CR := Temp_CR;
   end Select_Current_Memory_Buffer;

   ------------------------------------
   -- Configure_Double_Buffered_Mode --
   ------------------------------------

   procedure Configure_Double_Buffered_Mode
     (Unit              : in out DMA_Controller;
      Stream            : DMA_Stream_Selector;
      Buffer_0_Value    : Address;
      Buffer_1_Value    : Address;
      First_Buffer_Used : Memory_Buffer_Target)
   is
      This_Stream : DMA_Stream renames Unit.Streams (Stream);
      Temp_CR     : Stream_Config_Register := This_Stream.CR;
   begin
      This_Stream.M0AR := Buffer_0_Value;
      This_Stream.M1AR := Buffer_1_Value;
      Temp_CR.Current_Target := First_Buffer_Used;
      Unit.Streams (Stream).CR := Temp_CR;
   end Configure_Double_Buffered_Mode;

   ---------------------------------
   -- Enable_Double_Buffered_Mode --
   ---------------------------------

   procedure Enable_Double_Buffered_Mode
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)
   is
      Temp_CR : Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      Temp_CR.Double_Buffered := True;
      Unit.Streams (Stream).CR := Temp_CR;
   end Enable_Double_Buffered_Mode;

   ----------------------------------
   -- Disable_Double_Buffered_Mode --
   ----------------------------------

   procedure Disable_Double_Buffered_Mode
     (Unit   : in out DMA_Controller;
      Stream : DMA_Stream_Selector)is
      Temp_CR : Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      Temp_CR.Double_Buffered := False;
      Unit.Streams (Stream).CR := Temp_CR;
   end Disable_Double_Buffered_Mode;

   ----------------------
   -- Selected_Channel --
   ----------------------

   function Selected_Channel
     (Unit : DMA_Controller;  Stream : DMA_Stream_Selector)
      return DMA_Channel_Selector
   is
      Current : constant Stream_Config_Register := Unit.Streams (Stream).CR;
   begin
      return Current.Channel;
   end Selected_Channel;

   -------------
   -- Aligned --
   -------------

   function Aligned (This : Address;  Width : DMA_Data_Transfer_Widths)
      return Boolean
   is
      use System.Storage_Elements;
   begin
      case Width is
         when Words =>
            return To_Integer (This) mod 4 = 0;
         when HalfWords =>
            return To_Integer (This) mod 2 = 0;
         when Bytes =>
            return True;
      end case;
   end Aligned;

end STM32F4.DMA;
