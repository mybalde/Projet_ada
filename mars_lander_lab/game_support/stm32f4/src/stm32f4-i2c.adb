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

with STM32F4.RCC;   use STM32F4.RCC;
with Ada.Real_Time; use Ada.Real_Time;

package body STM32F4.I2C is

   subtype I2C_SR1_Flag is I2C_Status_Flag range Start_Bit .. SMB_Alert;
   subtype I2C_SR2_Flag is I2C_Status_Flag range Master_Slave_Mode .. Dual_Flag;

   SR1_Flag_Pos : constant array (I2C_SR1_Flag) of Natural :=
     (0, 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 14, 15);

   SR2_Flag_Pos : constant array (I2C_SR2_Flag) of Natural :=
     (0, 1, 2, 4, 5, 6, 7);

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Port        : in out I2C_Port;
      Clock_Speed : Word;
      Mode        : I2C_Device_Mode;
      Duty_Cycle  : I2C_Duty_Cycle;
      Own_Address : Half_Word;
      Ack         : I2C_Acknowledgement;
      Ack_Address : I2C_Acknowledge_Address)
   is
      CR2, CR1   : Half_Word;
      CCR        : Half_Word := 0;
      PCLK1      : constant Word := System_Clock_Frequencies.PCLK1;
      Freq_Range : constant Half_Word := Half_Word (PCLK1 / 1_000_000);
   begin
      --  Load CR2 and clear FREQ
      CR2 := Port.CR2 and (not CR2_FREQ);

      Port.CR2 := CR2 or Freq_Range;

      Set_State (Port, Disabled);

      if Clock_Speed <= 100_000 then
         CCR := Half_Word (PCLK1 / (Clock_Speed * 2));

         if CCR < 4 then
            CCR := 4;
         end if;
         Port.TRISE := Freq_Range + 1;
      else
         --  Fast mode

         if Duty_Cycle = DutyCycle_2 then
            CCR := Half_Word (PCLK1 / (Clock_Speed * 3));
         else
            CCR := Half_Word (PCLK1 / (Clock_Speed * 25));
            CCR := CCR or DutyCycle_16_9'Enum_Rep;
         end if;

         if (CCR and CCR_CCR) = 0 then
            CCR := 1;
         end if;

         CCR := CCR or CCR_FS;

         Port.TRISE := (Freq_Range * 300) / 1000 + 1;
      end if;

      Port.CCR := CCR;

      Set_State (Port, Enabled);

      CR1 := Port.CR1;

      CR1 := CR1 and CR1_Clear_Mask;

      CR1 := CR1 or Mode'Enum_Rep or Ack'Enum_Rep;
      Port.CR1 := CR1;

      Port.OAR1 := Ack_Address'Enum_Rep or Own_Address'Enum_Rep;
   end Configure;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State (Port : in out I2C_Port; State : I2C_State) is
   begin
      if State = Enabled then
         Port.CR1 := Port.CR1 or CR1_PE;
      else
         Port.CR1 := Port.CR1 and (not CR1_PE);
      end if;
   end Set_State;

   ------------------
   -- Port_Enabled --
   ------------------

   function Port_Enabled (Port : I2C_Port) return Boolean is
   begin
      return (Port.CR1 and CR1_PE) /= 0;
   end Port_Enabled;

   --------------------
   -- Generate_Start --
   --------------------

   procedure Generate_Start (Port : in out I2C_Port; State : I2C_State) is
   begin
      if State = Enabled then
         Port.CR1 := Port.CR1 or CR1_START;
      else
         Port.CR1 := Port.CR1 and (not CR1_START);
      end if;
   end Generate_Start;

   -------------------
   -- Generate_Stop --
   -------------------

   procedure Generate_Stop (Port : in out I2C_Port; State : I2C_State) is
   begin
      if State = Enabled then
         Port.CR1 := Port.CR1 or CR1_STOP;
      else
         Port.CR1 := Port.CR1 and (not CR1_STOP);
      end if;
   end Generate_Stop;

   --------------------
   -- Send_7Bit_Addr --
   --------------------

   procedure Send_7Bit_Address
     (Port      : in out I2C_Port;
      Address   : Byte;
      Direction : I2C_Direction)
   is
      Destination : Half_Word := Half_Word (Address);
   begin
      if Direction = Receiver then
         Destination := Destination or I2C_OAR1_ADD0;
      else
         Destination := Destination and (not I2C_OAR1_ADD0);
      end if;

      Port.DR := Destination;
   end Send_7Bit_Address;

   --------------
   -- Get_Flag --
   --------------

   function Status (Port : I2C_Port; Flag : I2C_Status_Flag) return Boolean is
   begin
      if Flag in I2C_SR1_Flag then
         return (Port.SR1 and (2**SR1_Flag_Pos (Flag))) /= 0;
      else
         return (Port.SR2 and (2**SR2_Flag_Pos (Flag))) /= 0;
      end if;
   end Status;

   ----------------
   -- Clear_Flag --
   ----------------

   procedure Clear_Status
     (Port   : in out I2C_Port;
      Target : Clearable_I2C_Status_Flag)
   is
   begin
      --  note that only a subset of the status flags can be cleared
      Port.SR1 := not (2 ** SR1_Flag_Pos (Target));
      --  we do not logically AND status bits
   end Clear_Status;

   -------------------------------
   -- Clear_Address_Sent_Status --
   -------------------------------

   procedure Clear_Address_Sent_Status (Port : in out I2C_Port) is
      Temp : Half_Word with Volatile;
      ADDR_Mask : constant Half_Word := 2 ** SR1_Flag_Pos (Address_Sent);
      STOP_Mask : constant Half_Word := 2 ** SR1_Flag_Pos (Stop_Detection);
   begin
      --  To clear the ADDR flag we have to read SR2 after reading SR1.
      --  However, per the RM, section 27.6.7, page 850, we should only read
      --  SR2 if the Address_Sent flag is set in SR1, or if the Stop_Detection
      --  flag is cleared, because the Address_Sent flag could be set in
      --  the middle of reading SR1 and SR2 but will be cleared by the
      --  read sequence nonetheless.
      Temp := Port.SR1;
      if ((Temp and ADDR_Mask) = 1) or ((Temp and STOP_Mask) = 0) then
         Temp := Port.SR2;
      end if;
   end Clear_Address_Sent_Status;

   ---------------------------------
   -- Clear_Stop_Detection_Status --
   ---------------------------------

   procedure Clear_Stop_Detection_Status (Port : in out I2C_Port) is
      Temp : Half_Word with Volatile, Unreferenced;
   begin
      Temp := Port.SR1;
      Port.CR1 := Port.CR1 or CR1_PE;
   end Clear_Stop_Detection_Status;

   -------------------
   -- Wait_For_Flag --
   -------------------

   procedure Wait_For_State
     (Port     : I2C_Port;
      Queried  : I2C_Status_Flag;
      State    : I2C_State;
      Time_Out : Natural := 1_000_000)
   is
      Expected : constant Boolean := State = Enabled;
      Deadline : constant Time := Clock + Milliseconds (Time_Out);
   begin
      while Status (Port, Queried) /= Expected loop
         if Clock >= Deadline then
            raise I2C_Timeout;
         end if;
      end loop;
   end Wait_For_State;

   ---------------
   -- Send_Data --
   ---------------

   procedure Send_Data (Port : in out I2C_Port; Data : Byte) is
   begin
      Port.DR := Half_Word (Data);
   end Send_Data;

   ---------------
   -- Read_Data --
   ---------------

   function Read_Data (Port : I2C_Port) return Byte is
   begin
      return Byte (Port.DR);
   end Read_Data;

   --------------------
   -- Set_Ack_Config --
   --------------------

   procedure Set_Ack_Config (Port : in out I2C_Port; State : I2C_State) is
   begin
      if State = Enabled then
         Port.CR1 := Port.CR1 or CR1_ACK;
      else
         Port.CR1 := Port.CR1 and (not CR1_ACK);
      end if;
   end Set_Ack_Config;

   ---------------------
   -- Set_Nack_Config --
   ---------------------

   procedure Set_Nack_Config
     (Port : in out I2C_Port;
      Pos  : I2C_Nack_Position)
   is
   begin
      if Pos = Next then
         Port.CR1 := Port.CR1 or CR1_POS;
      else
         Port.CR1 := Port.CR1 and (not CR1_POS);
      end if;
   end Set_Nack_Config;

   -----------
   -- Start --
   -----------

   procedure Start
     (Port      : in out I2C_Port;
      Address   : Byte;
      Direction : I2C_Direction)
   is
   begin
      Generate_Start (Port, Enabled);
      Wait_For_State (Port, Start_Bit, Enabled);

      Set_Ack_Config (Port, Enabled);

      Send_7Bit_Address (Port, Address, Direction);

      while not Status (Port, Address_Sent) loop
         if Status (Port, Ack_Failure) then
            raise Program_Error;
         end if;
      end loop;
      Clear_Address_Sent_Status (Port);
   end Start;

   --------------
   -- Read_Ack --
   --------------

   function Read_Ack (Port : in out I2C_Port) return Byte is
   begin
      Set_Ack_Config (Port, Enabled);
      Wait_For_State (Port, Rx_Data_Register_Not_Empty, Enabled);
      return Read_Data (Port);
   end Read_Ack;

   ---------------
   -- Read_Nack --
   ---------------

   function Read_Nack (Port : in out I2C_Port) return Byte is
   begin
      Set_Ack_Config (Port, Disabled);
      Generate_Stop (Port, Enabled);
      Wait_For_State (Port, Rx_Data_Register_Not_Empty, Enabled);
      return Read_Data (Port);
   end Read_Nack;

   -----------
   -- Write --
   -----------

   procedure Write (Port : in out I2C_Port; Data : Byte) is
   begin
      Wait_For_State (Port, Tx_Data_Register_Empty, Enabled);
      Send_Data (Port, Data);

      while
        not Status (Port, Tx_Data_Register_Empty) or else
        not Status (Port, Byte_Transfer_Finished)
      loop
         null;
      end loop;
   end Write;

   ----------
   -- Stop --
   ----------

   procedure Stop (Port : in out I2C_Port) is
   begin
      Generate_Stop (Port, Enabled);
   end Stop;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (Port   : in out I2C_Port;
      Source : I2C_Interrupt)
   is
   begin
      Port.CR2 := Port.CR2 or Source'Enum_Rep;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (Port   : in out I2C_Port;
      Source : I2C_Interrupt)
   is
   begin
      Port.CR2 := Port.CR2 and not Source'Enum_Rep;
   end Disable_Interrupt;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (Port   : in out I2C_Port;
      Source : I2C_Interrupt)
      return Boolean
   is
   begin
      return (Port.CR2 and Source'Enum_Rep) = Source'Enum_Rep;
   end Enabled;

end STM32F4.I2C;
