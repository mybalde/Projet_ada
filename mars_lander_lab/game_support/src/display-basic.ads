-----------------------------------------------------------------------
--                              Ada Labs                             --
--                                                                   --
--                 Copyright (C) 2008-2013, AdaCore                  --
--                                                                   --
-- Labs is free  software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

package Display.Basic is


   type Window_ID is private;

   function Create_Window (Width : Integer; Height : Integer; Name : String) return Window_ID;

   Graphical_Context_Not_Initialized : exception;
   Too_Many_Windows: exception;
   Too_Many_Canvas : exception;

   type Canvas_ID is  private;

   function Get_Canvas(Window : Window_ID) return Canvas_ID;

   type Point_2d is record
      X : Float;
      Y : Float;
   end record;

   type Point_3d is record
      X : Float;
      Y : Float;
      Z : Float;
   end record;

   type Screen_Point is record
      X, Y : Integer;
   end record;

   type Cursor_T is record
      Position : Screen_Point;
      Pressed : Boolean;
   end record;

   type Key_T is
     (SDLK_UNKNOWN,
      SDLK_A,
      SDLK_B,
      SDLK_C,
      SDLK_D,
      SDLK_E,
      SDLK_F,
      SDLK_G,
      SDLK_H,
      SDLK_I,
      SDLK_J,
      SDLK_K,
      SDLK_L,
      SDLK_M,
      SDLK_N,
      SDLK_O,
      SDLK_P,
      SDLK_Q,
      SDLK_R,
      SDLK_S,
      SDLK_T,
      SDLK_U,
      SDLK_V,
      SDLK_W,
      SDLK_X,
      SDLK_Y,
      SDLK_Z,
      SDLK_1,
      SDLK_2,
      SDLK_3,
      SDLK_4,
      SDLK_5,
      SDLK_6,
      SDLK_7,
      SDLK_8,
      SDLK_9,
      SDLK_0,
      SDLK_RETURN,
      SDLK_ESCAPE,
      SDLK_BACKSPACE,
      SDLK_TAB,
      SDLK_SPACE,
      SDLK_MINUS,
      SDLK_EQUALS,
      SDLK_LEFTBRACKET,
      SDLK_RIGHTBRACKET,
      SDLK_BACKSLASH,
      SDLK_NONUSHASH,
      SDLK_SEMICOLON,
      SDLK_APOSTROPHE,
      SDLK_GRAVE,
      SDLK_COMMA,
      SDLK_PERIOD,
      SDLK_SLASH,
      SDLK_CAPSLOCK,
      SDLK_F1,
      SDLK_F2,
      SDLK_F3,
      SDLK_F4,
      SDLK_F5,
      SDLK_F6,
      SDLK_F7,
      SDLK_F8,
      SDLK_F9,
      SDLK_F10,
      SDLK_F11,
      SDLK_F12,
      SDLK_PRINTSCREEN,
      SDLK_SCROLLLOCK,
      SDLK_PAUSE,
      SDLK_INSERT,
      SDLK_HOME,
      SDLK_PAGEUP,
      SDLK_DELETE,
      SDLK_END,
      SDLK_PAGEDOWN,
      SDLK_RIGHT,
      SDLK_LEFT,
      SDLK_DOWN,
      SDLK_UP,
      SDLK_NUMLOCKCLEAR,
      SDLK_KP_DIVIDE,
      SDLK_KP_MULTIPLY,
      SDLK_KP_MINUS,
      SDLK_KP_PLUS,
      SDLK_KP_ENTER,
      SDLK_KP_1,
      SDLK_KP_2,
      SDLK_KP_3,
      SDLK_KP_4,
      SDLK_KP_5,
      SDLK_KP_6,
      SDLK_KP_7,
      SDLK_KP_8,
      SDLK_KP_9,
      SDLK_KP_0,
      SDLK_KP_PERIOD,
      SDLK_NONUSBACKSLASH,
      SDLK_APPLICATION,
      SDLK_POWER,
      SDLK_KP_EQUALS,
      SDLK_F13,
      SDLK_F14,
      SDLK_F15,
      SDLK_F16,
      SDLK_F17,
      SDLK_F18,
      SDLK_F19,
      SDLK_F20,
      SDLK_F21,
      SDLK_F22,
      SDLK_F23,
      SDLK_F24,
      SDLK_EXECUTE,
      SDLK_HELP,
      SDLK_MENU,
      SDLK_SELECT,
      SDLK_STOP,
      SDLK_AGAIN,
      SDLK_UNDO,
      SDLK_CUT,
      SDLK_COPY,
      SDLK_PASTE,
      SDLK_FIND,
      SDLK_MUTE,
      SDLK_VOLUMEUP,
      SDLK_VOLUMEDOWN,
      SDLK_KP_COMMA,
      SDLK_KP_EQUALSAS400,
      SDLK_INTERNATIONAL1,
      SDLK_INTERNATIONAL2,
      SDLK_INTERNATIONAL3,
      SDLK_INTERNATIONAL4,
      SDLK_INTERNATIONAL5,
      SDLK_INTERNATIONAL6,
      SDLK_INTERNATIONAL7,
      SDLK_INTERNATIONAL8,
      SDLK_INTERNATIONAL9,
      SDLK_LANG1,
      SDLK_LANG2,
      SDLK_LANG3,
      SDLK_LANG4,
      SDLK_LANG5,
      SDLK_LANG6,
      SDLK_LANG7,
      SDLK_LANG8,
      SDLK_LANG9,
      SDLK_ALTERASE,
      SDLK_SYSREQ,
      SDLK_CANCEL,
      SDLK_CLEAR,
      SDLK_PRIOR,
      SDLK_RETURN2,
      SDLK_SEPARATOR,
      SDLK_OUT,
      SDLK_OPER,
      SDLK_CLEARAGAIN,
      SDLK_CRSEL,
      SDLK_EXSEL,
      SDLK_KP_00,
      SDLK_KP_000,
      SDLK_THOUSANDSSEPARATOR,
      SDLK_DECIMALSEPARATOR,
      SDLK_CURRENCYUNIT,
      SDLK_CURRENCYSUBUNIT,
      SDLK_KP_LEFTPAREN,
      SDLK_KP_RIGHTPAREN,
      SDLK_KP_LEFTBRACE,
      SDLK_KP_RIGHTBRACE,
      SDLK_KP_TAB,
      SDLK_KP_BACKSPACE,
      SDLK_KP_A,
      SDLK_KP_B,
      SDLK_KP_C,
      SDLK_KP_D,
      SDLK_KP_E,
      SDLK_KP_F,
      SDLK_KP_XOR,
      SDLK_KP_POWER,
      SDLK_KP_PERCENT,
      SDLK_KP_LESS,
      SDLK_KP_GREATER,
      SDLK_KP_AMPERSAND,
      SDLK_KP_DBLAMPERSAND,
      SDLK_KP_VERTICALBAR,
      SDLK_KP_DBLVERTICALBAR,
      SDLK_KP_COLON,
      SDLK_KP_HASH,
      SDLK_KP_SPACE,
      SDLK_KP_AT,
      SDLK_KP_EXCLAM,
      SDLK_KP_MEMSTORE,
      SDLK_KP_MEMRECALL,
      SDLK_KP_MEMCLEAR,
      SDLK_KP_MEMADD,
      SDLK_KP_MEMSUBTRACT,
      SDLK_KP_MEMMULTIPLY,
      SDLK_KP_MEMDIVIDE,
      SDLK_KP_PLUSMINUS,
      SDLK_KP_CLEAR,
      SDLK_KP_CLEARENTRY,
      SDLK_KP_BINARY,
      SDLK_KP_OCTAL,
      SDLK_KP_DECIMAL,
      SDLK_KP_HEXADECIMAL,
      SDLK_LCTRL,
      SDLK_LSHIFT,
      SDLK_LALT,
      SDLK_LGUI,
      SDLK_RCTRL,
      SDLK_RSHIFT,
      SDLK_RALT,
      SDLK_RGUI,
      SDLK_MODE,
      SDLK_AUDIONEXT,
      SDLK_AUDIOPREV,
      SDLK_AUDIOSTOP,
      SDLK_AUDIOPLAY,
      SDLK_AUDIOMUTE,
      SDLK_MEDIASELECT,
      SDLK_WWW,
      SDLK_MAIL,
      SDLK_CALCULATOR,
      SDLK_COMPUTER,
      SDLK_AC_SEARCH,
      SDLK_AC_HOME,
      SDLK_AC_BACK,
      SDLK_AC_FORWARD,
      SDLK_AC_STOP,
      SDLK_AC_REFRESH,
      SDLK_AC_BOOKMARKS,
      SDLK_BRIGHTNESSDOWN,
      SDLK_BRIGHTNESSUP,
      SDLK_DISPLAYSWITCH,
      SDLK_KBDILLUMTOGGLE,
      SDLK_KBDILLUMDOWN,
      SDLK_KBDILLUMUP,
      SDLK_EJECT,
      SDLK_SLEEP,
      SDLK_APP1,
      SDLK_APP2,
      SDLK_AUDIOREWIND,
      SDLK_AUDIOFASTFORWARD,
      SDLK_NUM_SCANCODES);

   procedure Fill(Canvas : Canvas_ID; Color: RGBA_T);
   procedure Draw_Sphere (Canvas : Canvas_ID; Position: Point_3d; Radius : Float; Color: RGBA_T);
   procedure Draw_Sphere (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T);
   procedure Draw_Circle (Canvas : Canvas_ID; Position: Point_3d; Radius : Float; Color: RGBA_T);
   procedure Draw_Circle (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T);
   procedure Draw_Line (Canvas : Canvas_ID; P1: Point_3d; P2 : Point_3d; Color: RGBA_T);
   procedure Draw_Line (Canvas : Canvas_ID; P1: Screen_Point; P2 : Screen_Point; Color: RGBA_T);
   procedure Draw_Text (Canvas : Canvas_ID; Position: Point_3d; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True);
   procedure Draw_Text (Canvas : Canvas_ID; Position: Screen_Point; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True);
   procedure Draw_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T);
   procedure Draw_Rect (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T);
   procedure Draw_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Rotation : Float; Color : RGBA_T);
   procedure Draw_Image (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Rotation : Float; Image : Image_T);
   procedure Draw_Image (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Image : Image_T);
   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T);
   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T);
   procedure Set_Pixel (Canvas : Canvas_ID; Position : Screen_Point; Color : RGBA_T);

   procedure Enable_3d_Light (Canvas : Canvas_ID);
   procedure Disable_3d_Light (Canvas : Canvas_ID);
   procedure Set_3d_Light (Canvas : Canvas_ID;
                           Position : Point_3d;
                           Diffuse_Color : RGBA_T;
                           Ambient_Color : RGBA_T);



   function Get_Text_Size(Text : String) return Screen_Point;

   function Get_Zoom_Factor (Canvas : Canvas_ID) return Float;
   procedure Set_Zoom_Factor (Canvas : Canvas_ID; ZF : Float);

   procedure Set_Center (Canvas : Canvas_ID; Position : Point_3d);
   procedure Set_Center (Canvas : Canvas_ID; Position : Screen_Point);
   function Get_Center (Canvas : Canvas_ID) return Screen_Point;
   function Get_Canvas_Size(Canvas : Canvas_ID) return Screen_Point;
   procedure Swap_Buffers(Window : Window_ID; Erase : Boolean := True);
   --  Update the canvas

   -- copy the hidden buffer to the visible buffer
   procedure Swap_Copy_Buffers(Window : Window_ID);
   --  Update the canvas

   function Get_Cursor_Status return Cursor_T;
   function Get_Key_Status (Key : Key_T) return Boolean;

   function To_Point3d (Canvas : Canvas_ID; P : Screen_Point) return Point_3d;
   function To_Screen_Point (Canvas : Canvas_ID;  P : Point_3d) return Screen_Point;

  function Is_Killed return Boolean;


private


   type Window_ID_Range is range 0 .. 10;
   type Window_ID is new Window_ID_Range;

   type Canvas_ID_Range is range 1 .. 1024;
   type Canvas_ID is new Canvas_ID_Range;


end Display.Basic;
