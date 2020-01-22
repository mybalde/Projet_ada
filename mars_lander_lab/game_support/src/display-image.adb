with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with System.Address_To_Access_Conversions;

with SDL_surface_h; use SDL_surface_h;
with SDL_pixels_h; use SDL_pixels_h;
with SDL_rwops_h; use SDL_rwops_h;
with SDL_stdinc_h; use SDL_stdinc_h;

with Ada.Unchecked_Deallocation;

package body Display.Image is

   package Pixel_Conversions is new
     System.Address_To_Access_Conversions (Uint32);

   package Indices_Conversions is new
     System.Address_To_Access_Conversions (Uint8);

   type Pixels_T is array (Natural range <>) of aliased Uint32;
   package Pixels_Pointers is new
     Interfaces.C.Pointers (Natural, Uint32, Pixels_T, 0);

   type Indices_T is array (Natural range <>) of aliased Uint8;
   package Indices_Pointers is new
     Interfaces.C.Pointers (Natural, Uint8, Indices_T, 0);

   type Color_Array is array (Uint8 range <>) of aliased SDL_Color;
   package Color_Pointers is new
     Interfaces.C.Pointers (Uint8, SDL_Color, Color_Array, SDL_Color'(0, 0, 0, 0));

   function Load_BMP (File : String) return Image_T is
      use type Interfaces.C.int;

      File_Char_Ptr : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (File);
      Mode_Char_Ptr : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("rb");
      Surface : access SDL_Surface :=
        SDL_LoadBMP_RW (SDL_RWFromFile (File_Char_Ptr, Mode_Char_Ptr), 1);
      Pixels : Image_T;
   begin
      if Surface = null then
         Put_Line ("Cannot open file");
         raise Program_Error;
      end if;

      Pixels := new RGBA_Array (1 .. Integer (Surface.h), 1 .. Integer (Surface.w));

      case Surface.Format.BytesPerPixel is
         when 1 =>
            --  One which is the index inside the format.palette.colors array
            declare
               use type Color_Pointers.Pointer;

               Surface_Pixels : Indices_Pointers.Pointer :=
                 Indices_Pointers.Pointer
                   (Indices_Conversions.To_Pointer (Surface.pixels));
               Colors : Color_Array :=
                 Color_Pointers.Value
                   (Color_Pointers.Pointer (Surface.format.palette.colors),
                    Interfaces.C.ptrdiff_t (Surface.format.palette.ncolors));
            begin
               for I in Integer range 1 .. Integer (Surface.h) loop
                  for J in Integer range 1 .. Integer (Surface.w) loop
                     declare
                        Color : SDL_Color := Colors (Surface_Pixels.all);
                     begin
                        Pixels (I, J) :=
                          (Color_Component_T (Color.R),
                           Color_Component_T (Color.G),
                           Color_Component_T (Color.B),
                           Color_Component_T (Color.A));

                        Indices_Pointers.Increment (Surface_Pixels);
                     end;
                  end loop;
               end loop;
            end;
         when 2 | 3 =>
            Put_Line ("Unable to read surface.");
            raise Program_Error;
         when 4 =>
            --  32 bit color inside pixel array directly
            declare
               Surface_Pixels : Pixels_Pointers.Pointer :=
                 Pixels_Pointers.Pointer
                   (Pixel_Conversions.To_Pointer (Surface.pixels));
            begin
               for I in Integer range 1 .. Integer (Surface.h) loop
                  for J in Integer range 1 .. Integer (Surface.w) loop
                     declare
                        R, G, B, A : aliased Uint8;
                     begin
                        SDL_GetRGBA
                          (Surface_Pixels.all,
                           Surface.format,
                           R'Access,
                           G'Access,
                           B'Access,
                           A'Access);

                        Pixels (I, J) :=
                          (Color_Component_T (R),
                           Color_Component_T (G),
                           Color_Component_T (B),
                           Color_Component_T (A));

                        Pixels_Pointers.Increment (Surface_Pixels);
                     end;
                  end loop;
               end loop;
            end;
         when others =>
            raise Program_Error;
      end case;

      Interfaces.C.Strings.Free (File_Char_Ptr);
      Interfaces.C.Strings.Free (Mode_Char_Ptr);

      return Pixels;
   end Load_BMP;

   procedure Image_Deallocation is new
     Ada.Unchecked_Deallocation (RGBA_Array, Image_T);

   procedure Free (X : in out Image_T) is
   begin
      Image_Deallocation (X);
   end Free;

end Display.Image;
