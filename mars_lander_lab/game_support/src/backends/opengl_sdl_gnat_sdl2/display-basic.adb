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

--with Display.Kernel;  use Display.Kernel;
with SDL_surface_h; use SDL_surface_h;
with SDL_video_h; use SDL_video_h;
with SDL_surface_h; use SDL_surface_h;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C.Pointers;
with SDL_error_h; use SDL_error_h;
with SDL_h; use SDL_h;
with SDL_events_h; use SDL_events_h;
with GL_gl_h; use GL_gl_h;
with GL_glu_h; use GL_glu_h;
with System;
use System;
with System.Address_To_Access_Conversions;
with Display.Basic.Utils; use Display.Basic.Utils;
with SDL_stdinc_h; use SDL_stdinc_h;
with SDL_pixels_h; use SDL_pixels_h;
with SDL_keyboard_h; use SDL_keyboard_h;
with SDL_scancode_h; use SDL_scancode_h;
with Display.Basic.Fonts; use Display.Basic.Fonts;
with Display.Basic.GLFonts; use Display.Basic.GLFonts;
package body Display.Basic is

   Initialized : boolean := False with Atomic, Volatile;

   type SDL_Window_Surface is record
      surface : Canvas_ID;
      window : access SDL_Window;
      w       : Integer := 400;
      h       : Integer := 400;
      bpp   :  Interfaces.C.int := 32;
    --  flags :  Interfaces.C.unsigned :=   SDL_HWSURFACE + SDL_RESIZABLE + SDL_DOUBLEBUF;
   end record;


   type Windows_Array is array (Window_ID) of SDL_Window_Surface;
   Nb_Windows : Integer := 0;

   Stored_Windows : Windows_Array;


   procedure Reshape (S : SDL_Window_Surface);
   procedure UpdateProjection(Canvas : Canvas_ID);

   ----------
   -- Draw --
   ----------

--     procedure Swap_Buffers(Window : Window_ID) is
--     begin
--        SDL_GL_SwapWindow (Stored_Windows(Window).window);
--     end Swap_Buffers;


   function Get_Zoom_Factor(Canvas : Canvas_ID) return Float is
   begin
      return  Get_Internal_Canvas(Canvas).Zoom_Factor;
   end Get_Zoom_Factor;

   procedure Set_Zoom_Factor (Canvas : Canvas_ID; ZF : Float) is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Display.Basic.Utils.Set_Zoom_Factor(Canvas, ZF);
    --  Reshape(Canvas, Integer(C.Surface.w), Integer(C.Surface.h));
      UpdateProjection(Canvas);
   end Set_Zoom_Factor;


   function Create_SDL_Window (Width : Integer; Height : Integer; Name : String) return SDL_Window_Surface;


   function Create_Window (Width : Integer; Height : Integer; Name : String) return Window_ID is
      Current_Id : Window_ID;
   begin
      if Nb_Windows = Windows_Array'Length then
         raise Too_Many_Windows;
      end if;
      Current_Id := Window_ID (Integer (Window_ID'First) + Nb_Windows);
      Stored_Windows(Current_Id) := Create_SDL_Window (Width, Height, Name);
      Nb_Windows := Nb_Windows + 1;
      return Current_Id;
   end Create_Window;

--     type T_Internal_Canvas is record
--        Surface : access SDL_Surface;
--        Zoom_Factor : Float := 1.0;
--        Center : Screen_Point := (0, 0);
--     end record;
--
--     type Internal_Canvas_Array is array (Canvas_ID) of T_Internal_Canvas;
--
--     Internal_Canvas : Internal_Canvas_Array;
--     Nb_Canvas : Integer := 0;
--
--
--     function Register_SDL_Surface(S : access SDL_Surface) return Canvas_ID is
--        Current_Id : Canvas_ID;
--     begin
--        if Nb_Canvas = Internal_Canvas'Length then
--           raise Too_Many_Canvas;
--        end if;
--
--        Current_Id := Canvas_ID(Integer(Internal_Canvas'First) + Nb_Canvas);
--        Internal_Canvas(Current_Id) := T_Internal_Canvas'(Surface     => S,
--                                                          Zoom_Factor => 1.0,
--                                                          Center => (0, 0));
--        Nb_Canvas := Nb_Canvas + 1;
--        return Current_Id;
--     end Register_SDL_Surface;

   Quadric     : System.Address;
   procedure UpdateProjection(Canvas : Canvas_ID) is
      C : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
      S : access SDL_Surface := C.Surface;
      Z : double := 1.0 / double (C.Zoom_Factor);
   begin
      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;
      glOrtho (-GLdouble(S.w / 2) * Z, GLdouble(S.w / 2) * Z,
               -GLdouble(S.h / 2) * Z, GLdouble(S.h / 2) * Z, -100.0, 300.0);
      glMatrixMode (GL_MODELVIEW);
      glLoadIdentity;

   end UpdateProjection;

   procedure Reshape (S : SDL_Window_Surface) is
      C : T_Internal_Canvas := Get_Internal_Canvas(S.surface);
      Z : double := 1.0;--double(C.Zoom_Factor);
   begin
      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;

      glViewport (0, 0, GLsizei (S.w), GLsizei (S.h));
      glOrtho (-GLdouble(S.w / 2) * Z, GLdouble(S.w / 2) * Z,
               -GLdouble(S.h / 2) * Z, GLdouble(S.h / 2) * Z, -100.0, 300.0);

      --gluPerspective(70.0,GLdouble(Width)/GLdouble(Height),1.0,1000.0);

      --      glOrtho (-GLdouble(Width / 2), GLdouble(Width / 2), -GLdouble(Height / 2), GLdouble(Height / 2), -100.0, 300.0);
--        gluLookAt (0.0, 0.0, -100.0,
--                   0.0, 0.0, 0.0,
--                   0.0, 1.0, 0.0);
--        Ratio := GLdouble (w) / GLdouble (h);
--
--        if w > h then
--           glOrtho (-100.0 * Ratio, 100.0 * Ratio, -100.0, 100.0, -100.0, 300.0);
--        else
--           glOrtho (-100.0, 100.0, -100.0 / Ratio, 100.0 / Ratio, -100.0, 300.0);
--        end if;
     -- glOrtho (-GLdouble(Width / 2), GLdouble(Width / 2), -GLdouble(Height / 2), GLdouble(Height / 2), -100.0, 300.0);


      glMatrixMode (GL_MODELVIEW);
      glLoadIdentity;

      Update_SDL_Surface(S.surface, SDL_GetWindowSurface(S.window));

   end Reshape;

   function Init_GL (S : SDL_Window_Surface) return Boolean is
      glcontext : SDL_GLContext;
      type GLFloat_Array is array (Natural range <>) of aliased GLfloat;
      light_diffuse : aliased GLFloat_Array
        := (1.0, 1.0, 1.0, 1.0);
      light_ambient : aliased GLFloat_Array
        := (0.2, 0.2, 0.2, 1.0);
      light_position : aliased GLFloat_Array
        := (0.0, 0.0, 0.0, 1.0);
      GL_MULTISAMPLE : constant :=  16#809D#;

   begin

      if SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1) /= 0 then
          Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
       end if;
       if SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24) /= 0 then
          Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
       end if;

--        if SDL_GL_SetAttribute( SDL_GL_MULTISAMPLEBUFFERS, 1 ) /= 0 then
--           Ada.Text_IO.Put_Line ("impossible d'initialiser SDL_GL_MULTISAMPLEBUFFERS à 1");
--        elsif SDL_GL_SetAttribute( SDL_GL_MULTISAMPLESAMPLES, 6 ) /= 0 then
--            Ada.Text_IO.Put_Line ("impossible d'initialiser SDL_GL_MULTISAMPLESAMPLES sur 6 buffers");
--        end if;

      if SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1) /= 0 then
         Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
      end if;
      if SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 4) /= 0 then
         Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
      end if;


      glcontext := SDL_GL_CreateContext(S.window);

      if System.Address (glcontext) = Null_Address then
             Ada.Text_IO.Put_Line ("Problem in creating GL context");
      end if;



      Reshape (S);



--        glCullFace( GL_BACK );
--        glFrontFace( GL_CCW );
--        glEnable( GL_CULL_FACE );
--

      glClearDepth(1.0);
      glShadeModel(GL_SMOOTH);
      glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
      glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
      glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
      glEnable(GL_LINE_SMOOTH);
--        glEnable(GL_POLYGON_SMOOTH);
      glEnable(GL_POINT_SMOOTH);
      glEnable(GL_MULTISAMPLE);
--           glDisable(GL_LINE_SMOOTH);
--        glDisable(GL_POLYGON_SMOOTH);
--        glDisable(GL_POINT_SMOOTH);
--        glEnable(GL_BLEND);
--        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
--  glFrontFace (GL_CW)

--  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
--        glEnable(GL_BLEND);
    --  glEnable(GL_POLYGON_SMOOTH);
--      glBlendFunc( GL_SRC_ALPHA_SATURATE, GL_ONE ) ;
      glEnable (GL_DEPTH_TEST);
    glEnable (GL_COLOR_MATERIAL);

--      glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
    -- lEnable (GL_NORMALIZE);
 --     glDepthFunc (GL_LESS);

--       glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);


       Quadric := gluNewQuadric;
      glEnable (GL_LIGHTING);
      glEnable (GL_LIGHT0);
      glLightfv (GL_LIGHT0, GL_AMBIENT, light_ambient (0)'Access);
      glLightfv (GL_LIGHT0, GL_DIFFUSE, light_diffuse (0)'Access);
      glLightfv (GL_LIGHT0, GL_POSITION, light_position (0)'Access);
   --  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,GL_TRUE);

      glClearColor (0.0, 0.0, 0.0, 1.0);
--glDisable(GL_CULL_FACE);


      --gluQuadricDrawStyle(Quadric, GLU_FILL);

      return True;
   end Init_GL;

   procedure Set_3d_Light (Canvas : Canvas_ID;
                           Position : Point_3d;
                           Diffuse_Color : RGBA_T;
                           Ambient_Color : RGBA_T) is
      type GLFloat_Array is array (Natural range <>) of aliased GLfloat;
      light_position : aliased GLFloat_Array
        := (Position.X, Position.Y, Position.Z, 1.0);
      light_diffuse : aliased GLFloat_Array
        := (Float (Diffuse_Color.R) / 255.0,
            Float (Diffuse_Color.G) / 255.0,
            Float (Diffuse_Color.B) / 255.0, 1.0);
      light_ambient : aliased GLFloat_Array :=
        (Float (Ambient_Color.R) / 255.0,
         Float (Ambient_Color.G) / 255.0,
         Float (Ambient_Color.B) / 255.0, 1.0);
      IC : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
   begin
      glPushMatrix;
--        glTranslated
--          (GLdouble (Position.X - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
--           GLdouble (Position.Y - (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
--           GLdouble (Position.Z));
      glTranslated
        (GLdouble ( - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
         GLdouble (- (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
         GLdouble (0.0));
      glLightfv (GL_LIGHT0, GL_POSITION, light_position (0)'Access);
      glLightfv (GL_LIGHT0, GL_DIFFUSE, light_diffuse (0)'Access);
      glLightfv (GL_LIGHT0, GL_AMBIENT, light_ambient (0)'Access);
      glPopMatrix;
   end Set_3d_Light;

   procedure Enable_3d_Light (Canvas : Canvas_ID) is
   begin
      glEnable (GL_LIGHTING);
      glEnable (GL_LIGHT0);
   end Enable_3d_Light;

   procedure Disable_3d_Light (Canvas : Canvas_ID) is
   begin
      glDisable (GL_LIGHTING);
      glDisable (GL_LIGHT0);
   end Disable_3d_Light;

   function Create_SDL_Window (Width : Integer; Height : Integer; Name : String) return SDL_Window_Surface is
      S : SDL_Window_Surface;
     SDL_S : access SDL_Surface;
   begin
--        Ada.Text_IO.Put_Line("Create_SDL_Window Entry ");
      if not Initialized then
         raise Graphical_Context_Not_Initialized;
      end if;

      --  To center a non-fullscreen window we need to set an environment
      --  variable
      if SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2) /= 0 then
             Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
      end if;

      if SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1) /= 0 then
          Ada.Text_IO.Put_Line ("Problem in GL SetAttribute");
      end if;
      S.window := SDL_CreateWindow(New_String(Name), SDL_WINDOWPOS_CENTERED,
                                 SDL_WINDOWPOS_CENTERED,
                                 Interfaces.C.int(Width),
                                  Interfaces.C.int(Height),
                                   SDL_WINDOW_OPENGL or
                                     SDL_WINDOW_SHOWN or
                                  SDL_WINDOW_RESIZABLE);



      S.w := Width;
      S.h := Height;


        SDL_S := SDL_GetWindowSurface(S.window);

      if SDL_S = null then
         Put_Line ("Error retrieving the window surface");
         Put_Line(Value (SDL_GetError));
         SDL_Quit;
         raise Graphical_Context_Not_Initialized;
      end if;

      S.surface := Register_SDL_Surface(SDL_S);

      if not Init_GL(S) then
         Ada.Text_IO.Put_Line ("Error in Init_GL");
      end if;

      return S;

   end Create_SDL_Window;




   function Get_Canvas(Window : Window_ID) return Canvas_ID is
   begin
      -- as there is only one opengl canvas per applcation in an sdl opengl context a constant is returned
      return Canvas_ID'First;
   end Get_Canvas;

   procedure Set_Color(Color : RGBA_T) is
   begin
      glColor3d (double(Color.R) / 255.0,
                 double(Color.G) / 255.0,
                 double(Color.B) / 255.0);
   end Set_Color;

   procedure Draw_Sphere (Canvas : Canvas_ID; Position : Point_3d; Radius : Float; Color: RGBA_T) is
      IC : T_Internal_Canvas := Get_Internal_Canvas (Canvas);
   begin
      glPushMatrix;

      glTranslated
        (GLdouble (Position.X - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Y - (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Z));

      Set_Color (Color);

      if Radius < 0.1 then
         Disable_3d_Light(Canvas);
         glBegin(GL_POINTS);
         --glColor3f(1,1,1);
         glVertex2i(0, 0);
         glEnd;
         Enable_3d_Light(Canvas);
      else
         gluSphere
           (qobj   => Quadric,
            radius => double(Radius),
            slices => 20,
            stacks => 20);
      end if;

      glPopMatrix;

   end Draw_Sphere;

   procedure Fill(Canvas : Canvas_ID; Color: RGBA_T) is null;
   procedure Draw_Sphere (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T) is null;

   procedure Draw_Circle (Canvas : Canvas_ID; Position: Point_3d; Radius : Float; Color: RGBA_T) is
            IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin

      glPushMatrix;

      glTranslated
        (GLdouble (Position.X - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Y - (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Z));

      Set_Color (Color);
      Disable_3d_Light(Canvas);

      glShadeModel(GL_SMOOTH);
      glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
      glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
      glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
      glEnable(GL_LINE_SMOOTH);
      glEnable(GL_POLYGON_SMOOTH);
      glEnable(GL_POINT_SMOOTH);

      gluDisk(Quadric, GLdouble(Radius), GLdouble(Radius + 1.0), 40, 40);


      Enable_3d_Light (Canvas);
      glPopMatrix;
   end Draw_Circle;

   procedure Draw_Circle (Canvas : Canvas_ID; Position: Screen_Point; Radius : Integer; Color: RGBA_T) is null;

   procedure Draw_Line (Canvas : Canvas_ID; P1: Point_3d; P2 : Point_3d; Color: RGBA_T) is
   begin
      glPushMatrix;

      Set_Color (Color);

      glBegin(GL_LINES);

--        glColor3ub(GLubyte (Color.R), GLubyte (Color.G), GLubyte (Color.B));

      glVertex3d(double (P1.X), double (P1.Y), double (P1.Z));
      glVertex3d(double (P2.X), double (P2.Y), double (P2.Z));

      glEnd;

      glPopMatrix;
   end Draw_Line;

   procedure Draw_Line (Canvas : Canvas_ID; P1: Screen_Point; P2 : Screen_Point; Color: RGBA_T) is null;



   procedure Draw_Text (Canvas : Canvas_ID; Position: Point_3d; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True) is
    --  S : access SDL_Surface;
      --Format : UInt32;
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
      Cursor : Screen_Point := (0, 0);
      Font   : BMP_Font := Font8x8;
      w : Integer := Char_Size (Font).X;
      h : Integer := Char_Size (Font).Y;


   begin
      glPushMatrix;

--        glTranslated
--          (GLdouble (Position.X),
--           GLdouble (Position.Y),
--           GLdouble (Position.Z));
      glTranslated
        (GLdouble (Position.X - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Y - (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Z));


      glScalef(1.0/IC.Zoom_Factor,
               1.0/IC.Zoom_Factor,
               1.0/IC.Zoom_Factor);

      --glMatrixMode(GL_PROJECTION);
      --glPushMatrix;
      --glLoadIdentity;
     -- glOrtho(0.0, double(IC.Surface.w),
       --         double(IC.Surface.h), 0.0, 0.0, 1.0);

      Disable_3d_Light(Canvas);
      glDisable(GL_DEPTH_TEST);

      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
--      glBlendFunc(GL_SRC_ALPHA, GL_ONE);--_MINUS_SRC_ALPHA);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

      Set_Color (Color);

      for C of Text loop
         glBindTexture(GL_TEXTURE_2D, getCharTexture(C));

         glBegin(GL_QUADS);
         glTexCoord2f(0.0, 1.0); glVertex3f(Float(Cursor.X), Float(Cursor.Y), 0.0);
         glTexCoord2f(0.0, 0.0); glVertex3f(Float(Cursor.X), Float(Cursor.Y) + Float(h), 0.0);
         glTexCoord2f(1.0, 0.0); glVertex3f(Float(Cursor.X) + Float(w), Float(Cursor.Y) + Float(h), 0.0);
         glTexCoord2f(1.0, 1.0); glVertex3f(Float(Cursor.X) + Float(w), Float(Cursor.Y), 0.0);
         glEnd;

         if Cursor.X + Char_Size (Font).X > Integer(IC.Surface.w) then
            if Wrap then
               Cursor.Y := Cursor.Y + h;
               Cursor.X := 0;
            else
               exit;
            end if;
         else
            Cursor.X := Cursor.X + w;
         end if;

      end loop;

      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
      glEnable(GL_DEPTH_TEST);
      Enable_3d_Light(Canvas);


--        glPopMatrix;
--        glMatrixMode(GL_MODELVIEW);
--        glLoadIdentity;

      glPopMatrix;
   end Draw_Text;

   procedure Draw_Text (Canvas : Canvas_ID; Position: Screen_Point; Text : String; Color: RGBA_T; Bg_Color : RGBA_T := Black; Wrap: Boolean := True) is
      --Format : UInt32 := SDL_PIXELFORMAT_RGBA32;
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
      Cursor : Screen_Point := Position;
      Font   : BMP_Font := Font8x8;
      w : Integer := Char_Size (Font).X;
      h : Integer := Char_Size (Font).Y;

   begin
      Disable_3d_Light(Canvas);
      glDisable(GL_DEPTH_TEST);

      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
--      glBlendFunc(GL_SRC_ALPHA, GL_ONE);--_MINUS_SRC_ALPHA);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glOrtho(0.0, double(IC.Surface.w),
                double(IC.Surface.h), 0.0, 0.0, 1.0);

      Set_Color (Color);

      for C of Text loop
         glBindTexture(GL_TEXTURE_2D, getCharTexture(C));

         glBegin(GL_QUADS);
         glTexCoord2f(0.0, 0.0); glVertex3f(Float(Cursor.X), Float(Cursor.Y), 0.0);
         glTexCoord2f(0.0, 1.0); glVertex3f(Float(Cursor.X), Float(Cursor.Y) + Float(h), 0.0);
         glTexCoord2f(1.0, 1.0); glVertex3f(Float(Cursor.X) + Float(w), Float(Cursor.Y) + Float(h), 0.0);
         glTexCoord2f(1.0, 0.0); glVertex3f(Float(Cursor.X) + Float(w), Float(Cursor.Y), 0.0);
         glEnd;

         if Cursor.X + Char_Size (Font).X > Integer(IC.Surface.w) then
            if Wrap then
               Cursor.Y := Cursor.Y + h;
               Cursor.X := 0;
            else
               exit;
            end if;
         else
            Cursor.X := Cursor.X + w;
         end if;

      end loop;

      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;

      glDisable(GL_TEXTURE_2D);
      glDisable(GL_BLEND);
      glEnable(GL_DEPTH_TEST);
      Enable_3d_Light(Canvas);

   end Draw_Text;

   procedure Draw_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T) is null;

   procedure Draw_Rect (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T) is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
       glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glOrtho(0.0, double(C.Surface.w),
                double(C.Surface.h), 0.0, 0.0, 1.0);
      Disable_3d_Light (Canvas);
      Set_Color (Color);

      glRecti(int (Position.X),
              int (Position.Y),
              int (Position.X + Width),
              int (Position.Y + Height));

      Enable_3d_Light (Canvas);
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
   end Draw_Rect;


   procedure Draw_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Rotation : Float; Color : RGBA_T) is
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      glPushMatrix;

      Set_Color (Color);

      glTranslated
        (GLdouble (Position.X - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Y - (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Z));
      glRotated (double (Rotation), 0.0, 0.0, 1.0);


      glBegin(GL_QUADS);

      glVertex2d((-double (Width) / 2.0), double (Height) / 2.0);
      glVertex2d((double (Width) / 2.0), double (Height) / 2.0);
      glVertex2d((double (Width) / 2.0), -double (Height) / 2.0);
      glVertex2d((-double (Width) / 2.0), -double (Height) / 2.0);

      glEnd;

      glPopMatrix;
   end Draw_Rect;

   type Texture_T is array (Positive range <>) of aliased GLfloat;
   package Texture_Pointers is new
     Interfaces.C.Pointers (Positive, GLfloat, Texture_T, 0.0);

   procedure Draw_Image (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Rotation : Float; Image : Image_T) is
      IC : T_Internal_Canvas;
   begin

      glPushMatrix;

      glColor4d(1.0, 1.0, 1.0, 1.0);

      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Image'Length(2), Image'Length(1), 0, GL_RGBA, GL_UNSIGNED_BYTE, Image (Image'First(1), Image'First(2))'Address);

      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, float (GL_LINEAR));
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, float (GL_LINEAR));
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, float (GL_CLAMP));
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, float (GL_CLAMP));

      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

      glTranslated
        (GLdouble (Position.X - (Float(IC.Center.X) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Y - (Float(IC.Center.Y) * (1.0/IC.Zoom_Factor))),
         GLdouble (Position.Z));
      glRotated (double (Rotation), 0.0, 0.0, 1.0);
      glScaled (double (Width), double (Height), 1.0);


      glBegin(GL_QUADS);

      --        glMaterialfv (GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, Blanc (1)'Access);

      glTexCoord2d(0.0, 0.0);
      glVertex2d(-0.5, 0.5);

      glTexCoord2d(1.0, 0.0);
      glVertex2d(0.5, 0.5);

      glTexCoord2d(1.0, 1.0);
      glVertex2d(0.5, -0.5);

      glTexCoord2d(0.0, 1.0);
      glVertex2d(-0.5, -0.5);

      glEnd;


      glDisable (GL_TEXTURE_2D);

      glPopMatrix;
   end Draw_Image;

   procedure Draw_Image (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Image : Image_T) is
   begin
      Draw_Image (Canvas, Position, Width, Height, 0.0, Image);
   end Draw_Image;


   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float; Color : RGBA_T) is
   begin
      null;
   end Draw_Fill_Rect;
   procedure Draw_Fill_Rect (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Integer; Color : RGBA_T) is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glOrtho(0.0, double(C.Surface.w),
                double(C.Surface.h), 0.0, 0.0, 1.0);
      Disable_3d_Light (Canvas);
      Set_Color (Color);

      glRecti(int (Position.X),
              int (Position.Y),
              int (Position.X + Width),
              int (Position.Y + Height));

      Enable_3d_Light (Canvas);
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
   end Draw_Fill_Rect;

   procedure Set_Pixel (Canvas : Canvas_ID; Position : Screen_Point; Color : RGBA_T) is null;
   function Get_Text_Size(Text : String) return Screen_Point is  begin
      return String_Size (Font8x8, Text);
   end Get_Text_Size;

   function Scale (Canvas: T_Internal_Canvas; L : Float) return Integer is (Integer (L * Canvas.Zoom_Factor));

   procedure Set_Center (Canvas : Canvas_ID; Position : Point_3d)is
       C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      Display.Basic.Utils.Set_Center(Canvas, (Scale(C, Position.X), Scale(C, Position.Y)));
   end Set_Center;

   procedure Set_Center (Canvas : Canvas_ID; Position : Screen_Point) is
   begin
       Display.Basic.Utils.Set_Center(Canvas, Position);
   end Set_Center;

   function Get_Center (Canvas : Canvas_ID) return Screen_Point  is
   begin
        return Display.Basic.Utils.Get_Center(Canvas);
   end Get_Center;

   function To_Point3d (Canvas : Canvas_ID; P : Screen_Point) return Point_3d is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      return (Float(P.X - Integer(C.Surface.w / 2) - C.Center.X),
              Float(Integer(C.Surface.h / 2) - C.Center.Y - P.Y),
              0.0);
   end To_Point3d;

   function To_Screen_Point (Canvas : Canvas_ID;  P : Point_3d) return Screen_Point is
      C : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      return Screen_Point'(X => Integer (C.Zoom_Factor * P.X) + Integer(C.Surface.w / 2) - C.Center.X,
                           Y => Integer(C.Surface.h / 2) + C.Center.Y - Integer (C.Zoom_Factor * P.Y));

   end To_Screen_Point;

   function Get_Canvas_Size(Canvas : Canvas_ID) return Screen_Point is
      IC : T_Internal_Canvas := Get_Internal_Canvas(Canvas);
   begin
      return Screen_Point'(Integer(IC.Surface.w), Integer(IC.Surface.h));
   end Get_Canvas_Size;

   Internal_Cursor : Cursor_T := ((0,0), False);


   type Keyboard_T is array (Key_T) of Boolean;
   Internal_Keyboard : Keyboard_T := (others => False);


    Killed : Boolean := False;

   function Is_Killed return Boolean is
   begin
      return Killed;
   end Is_Killed;

     procedure Poll_Events is
      E : aliased SDL_Event;
   begin
      while SDL_PollEvent (E'Access) /= 0 loop
         case unsigned (E.c_type) is
            when SDL_events_h.SDL_QUIT_Evt =>
               Killed := True;
               SDL_h.SDL_Quit;
            when SDL_events_h.SDL_MOUSEBUTTONDOWN =>
               Internal_Cursor.Position := (Integer(E.motion.x), Integer(E.motion.y));
               Internal_Cursor.Pressed := True;
            when SDL_events_h.SDL_MOUSEBUTTONUP =>
               Internal_Cursor.Position := (Integer(E.motion.x), Integer(E.motion.y));
               Internal_Cursor.Pressed := False;

            when SDL_events_h.SDL_WINDOWEVENT_Evt =>
               case E.window.event is
                  when SDL_WindowEventID'Pos(SDL_WINDOWEVENT_RESIZED) |
                       SDL_WindowEventID'Pos(SDL_WINDOWEVENT_SIZE_CHANGED) =>
                     Stored_Windows(Window_ID'First).w := Integer(E.window.data1);
                     Stored_Windows(Window_ID'First).h := Integer(E.window.data2);
                     Reshape (Stored_Windows(Window_ID'First));
                  when others =>
                     null;
               end case;

            when others =>
               null;
         end case;
      end loop;
   end Poll_Events;

   procedure Update_Keyboard_Status is
      type Uint8_Array is array (SDL_scancode_h.SDL_Scancode range <>) of aliased Uint8;

      package Ptr is new Interfaces.C.Pointers (SDL_scancode_h.SDL_Scancode,
                                                Uint8,
                                                Uint8_Array,
                                                Default_Terminator => -1);

      Keys_Length : aliased int;
      Keys_Array : Ptr.Pointer := SDL_GetKeyboardState (Keys_Length'Access);
   begin
      for I in SDL_Scancode range 0 .. SDL_Scancode (Keys_Length - 1) loop
         case I is
         when SDL_SCANCODE_UNKNOWN =>
            Internal_Keyboard (SDLK_UNKNOWN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_A =>
            Internal_Keyboard (SDLK_A) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_B =>
            Internal_Keyboard (SDLK_B) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_C =>
            Internal_Keyboard (SDLK_C) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_D =>
            Internal_Keyboard (SDLK_D) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_E =>
            Internal_Keyboard (SDLK_E) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F =>
            Internal_Keyboard (SDLK_F) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_G =>
            Internal_Keyboard (SDLK_G) := (if Keys_Array.all = 0 then False else True);
         when SDL_scancode_h.SDL_SCANCODE_H =>
            Internal_Keyboard (SDLK_H) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_I =>
            Internal_Keyboard (SDLK_I) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_J =>
            Internal_Keyboard (SDLK_J) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_K =>
            Internal_Keyboard (SDLK_K) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_L =>
            Internal_Keyboard (SDLK_L) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_M =>
            Internal_Keyboard (SDLK_M) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_N =>
            Internal_Keyboard (SDLK_N) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_O =>
            Internal_Keyboard (SDLK_O) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_P =>
            Internal_Keyboard (SDLK_P) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_Q =>
            Internal_Keyboard (SDLK_Q) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_R =>
            Internal_Keyboard (SDLK_R) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_S =>
            Internal_Keyboard (SDLK_S) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_T =>
            Internal_Keyboard (SDLK_T) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_U =>
            Internal_Keyboard (SDLK_U) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_V =>
            Internal_Keyboard (SDLK_V) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_W =>
            Internal_Keyboard (SDLK_W) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_X =>
            Internal_Keyboard (SDLK_X) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_Y =>
            Internal_Keyboard (SDLK_Y) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_Z =>
            Internal_Keyboard (SDLK_Z) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_1 =>
            Internal_Keyboard (SDLK_1) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_2 =>
            Internal_Keyboard (SDLK_2) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_3 =>
            Internal_Keyboard (SDLK_3) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_4 =>
            Internal_Keyboard (SDLK_4) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_5 =>
            Internal_Keyboard (SDLK_5) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_6 =>
            Internal_Keyboard (SDLK_6) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_7 =>
            Internal_Keyboard (SDLK_7) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_8 =>
            Internal_Keyboard (SDLK_8) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_9 =>
            Internal_Keyboard (SDLK_9) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_0 =>
            Internal_Keyboard (SDLK_0) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_RETURN =>
            Internal_Keyboard (SDLK_RETURN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_ESCAPE =>
            Internal_Keyboard (SDLK_ESCAPE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_BACKSPACE =>
            Internal_Keyboard (SDLK_BACKSPACE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_TAB =>
            Internal_Keyboard (SDLK_TAB) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_SPACE =>
            Internal_Keyboard (SDLK_SPACE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_MINUS =>
            Internal_Keyboard (SDLK_MINUS) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_EQUALS =>
            Internal_Keyboard (SDLK_EQUALS) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LEFTBRACKET =>
            Internal_Keyboard (SDLK_LEFTBRACKET) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_RIGHTBRACKET =>
            Internal_Keyboard (SDLK_RIGHTBRACKET) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_BACKSLASH =>
            Internal_Keyboard (SDLK_BACKSLASH) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_NONUSHASH =>
            Internal_Keyboard (SDLK_NONUSHASH) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_SEMICOLON =>
            Internal_Keyboard (SDLK_SEMICOLON) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_APOSTROPHE =>
            Internal_Keyboard (SDLK_APOSTROPHE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_GRAVE =>
            Internal_Keyboard (SDLK_GRAVE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_COMMA =>
            Internal_Keyboard (SDLK_COMMA) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_PERIOD =>
            Internal_Keyboard (SDLK_PERIOD) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_SLASH =>
            Internal_Keyboard (SDLK_SLASH) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_CAPSLOCK =>
            Internal_Keyboard (SDLK_CAPSLOCK) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F1 =>
            Internal_Keyboard (SDLK_F1) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F2 =>
            Internal_Keyboard (SDLK_F2) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F3 =>
            Internal_Keyboard (SDLK_F3) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F4 =>
            Internal_Keyboard (SDLK_F4) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F5 =>
            Internal_Keyboard (SDLK_F5) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F6 =>
            Internal_Keyboard (SDLK_F6) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F7 =>
            Internal_Keyboard (SDLK_F7) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F8 =>
            Internal_Keyboard (SDLK_F8) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F9 =>
            Internal_Keyboard (SDLK_F9) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F10 =>
            Internal_Keyboard (SDLK_F10) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F11 =>
            Internal_Keyboard (SDLK_F11) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F12 =>
            Internal_Keyboard (SDLK_F12) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_PRINTSCREEN =>
            Internal_Keyboard (SDLK_PRINTSCREEN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_SCROLLLOCK =>
            Internal_Keyboard (SDLK_SCROLLLOCK) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_PAUSE =>
            Internal_Keyboard (SDLK_PAUSE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_INSERT =>
            Internal_Keyboard (SDLK_INSERT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_HOME =>
            Internal_Keyboard (SDLK_HOME) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_PAGEUP =>
            Internal_Keyboard (SDLK_PAGEUP) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_DELETE =>
            Internal_Keyboard (SDLK_DELETE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_END =>
            Internal_Keyboard (SDLK_END) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_PAGEDOWN =>
            Internal_Keyboard (SDLK_PAGEDOWN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_RIGHT =>
            Internal_Keyboard (SDLK_RIGHT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LEFT =>
            Internal_Keyboard (SDLK_LEFT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_DOWN =>
            Internal_Keyboard (SDLK_DOWN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_UP =>
            Internal_Keyboard (SDLK_UP) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_NUMLOCKCLEAR =>
            Internal_Keyboard (SDLK_NUMLOCKCLEAR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_DIVIDE =>
            Internal_Keyboard (SDLK_KP_DIVIDE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_MULTIPLY =>
            Internal_Keyboard (SDLK_KP_MULTIPLY) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_MINUS =>
            Internal_Keyboard (SDLK_KP_MINUS) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_PLUS =>
            Internal_Keyboard (SDLK_KP_PLUS) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_ENTER =>
            Internal_Keyboard (SDLK_KP_ENTER) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_1 =>
            Internal_Keyboard (SDLK_KP_1) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_2 =>
            Internal_Keyboard (SDLK_KP_2) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_3 =>
            Internal_Keyboard (SDLK_KP_3) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_4 =>
            Internal_Keyboard (SDLK_KP_4) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_5 =>
            Internal_Keyboard (SDLK_KP_5) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_6 =>
            Internal_Keyboard (SDLK_KP_6) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_7 =>
            Internal_Keyboard (SDLK_KP_7) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_8 =>
            Internal_Keyboard (SDLK_KP_8) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_9 =>
            Internal_Keyboard (SDLK_KP_9) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_0 =>
            Internal_Keyboard (SDLK_KP_0) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_PERIOD =>
            Internal_Keyboard (SDLK_KP_PERIOD) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_NONUSBACKSLASH =>
            Internal_Keyboard (SDLK_NONUSBACKSLASH) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_APPLICATION =>
            Internal_Keyboard (SDLK_APPLICATION) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_POWER =>
            Internal_Keyboard (SDLK_POWER) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_EQUALS =>
            Internal_Keyboard (SDLK_KP_EQUALS) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F13 =>
            Internal_Keyboard (SDLK_F13) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F14 =>
            Internal_Keyboard (SDLK_F14) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F15 =>
            Internal_Keyboard (SDLK_F15) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F16 =>
            Internal_Keyboard (SDLK_F16) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F17 =>
            Internal_Keyboard (SDLK_F17) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F18 =>
            Internal_Keyboard (SDLK_F18) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F19 =>
            Internal_Keyboard (SDLK_F19) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F20 =>
            Internal_Keyboard (SDLK_F20) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F21 =>
            Internal_Keyboard (SDLK_F21) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F22 =>
            Internal_Keyboard (SDLK_F22) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F23 =>
            Internal_Keyboard (SDLK_F23) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_F24 =>
            Internal_Keyboard (SDLK_F24) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_EXECUTE =>
            Internal_Keyboard (SDLK_EXECUTE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_HELP =>
            Internal_Keyboard (SDLK_HELP) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_MENU =>
            Internal_Keyboard (SDLK_MENU) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_SELECT =>
            Internal_Keyboard (SDLK_SELECT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_STOP =>
            Internal_Keyboard (SDLK_STOP) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AGAIN =>
            Internal_Keyboard (SDLK_AGAIN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_UNDO =>
            Internal_Keyboard (SDLK_UNDO) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_CUT =>
            Internal_Keyboard (SDLK_CUT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_COPY =>
            Internal_Keyboard (SDLK_COPY) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_PASTE =>
            Internal_Keyboard (SDLK_PASTE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_FIND =>
            Internal_Keyboard (SDLK_FIND) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_MUTE =>
            Internal_Keyboard (SDLK_MUTE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_VOLUMEUP =>
            Internal_Keyboard (SDLK_VOLUMEUP) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_VOLUMEDOWN =>
            Internal_Keyboard (SDLK_VOLUMEDOWN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_COMMA =>
            Internal_Keyboard (SDLK_KP_COMMA) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_EQUALSAS400 =>
            Internal_Keyboard (SDLK_KP_EQUALSAS400) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_INTERNATIONAL1 =>
            Internal_Keyboard (SDLK_INTERNATIONAL1) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_INTERNATIONAL2 =>
            Internal_Keyboard (SDLK_INTERNATIONAL2) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_INTERNATIONAL3 =>
            Internal_Keyboard (SDLK_INTERNATIONAL3) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_INTERNATIONAL4 =>
            Internal_Keyboard (SDLK_INTERNATIONAL4) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_INTERNATIONAL5 =>
            Internal_Keyboard (SDLK_INTERNATIONAL5) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_INTERNATIONAL6 =>
            Internal_Keyboard (SDLK_INTERNATIONAL6) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_INTERNATIONAL7 =>
            Internal_Keyboard (SDLK_INTERNATIONAL7) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_INTERNATIONAL8 =>
            Internal_Keyboard (SDLK_INTERNATIONAL8) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_INTERNATIONAL9 =>
            Internal_Keyboard (SDLK_INTERNATIONAL9) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LANG1 =>
            Internal_Keyboard (SDLK_LANG1) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LANG2 =>
            Internal_Keyboard (SDLK_LANG2) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LANG3 =>
            Internal_Keyboard (SDLK_LANG3) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LANG4 =>
            Internal_Keyboard (SDLK_LANG4) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LANG5 =>
            Internal_Keyboard (SDLK_LANG5) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LANG6 =>
            Internal_Keyboard (SDLK_LANG6) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LANG7 =>
            Internal_Keyboard (SDLK_LANG7) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LANG8 =>
            Internal_Keyboard (SDLK_LANG8) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LANG9 =>
            Internal_Keyboard (SDLK_LANG9) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_ALTERASE =>
            Internal_Keyboard (SDLK_ALTERASE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_SYSREQ =>
            Internal_Keyboard (SDLK_SYSREQ) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_CANCEL =>
            Internal_Keyboard (SDLK_CANCEL) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_CLEAR =>
            Internal_Keyboard (SDLK_CLEAR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_PRIOR =>
            Internal_Keyboard (SDLK_PRIOR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_RETURN2 =>
            Internal_Keyboard (SDLK_RETURN2) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_SEPARATOR =>
            Internal_Keyboard (SDLK_SEPARATOR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_OUT =>
            Internal_Keyboard (SDLK_OUT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_OPER =>
            Internal_Keyboard (SDLK_OPER) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_CLEARAGAIN =>
            Internal_Keyboard (SDLK_CLEARAGAIN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_CRSEL =>
            Internal_Keyboard (SDLK_CRSEL) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_EXSEL =>
            Internal_Keyboard (SDLK_EXSEL) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_00 =>
            Internal_Keyboard (SDLK_KP_00) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_000 =>
            Internal_Keyboard (SDLK_KP_000) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_THOUSANDSSEPARATOR =>
            Internal_Keyboard (SDLK_THOUSANDSSEPARATOR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_DECIMALSEPARATOR =>
            Internal_Keyboard (SDLK_DECIMALSEPARATOR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_CURRENCYUNIT =>
            Internal_Keyboard (SDLK_CURRENCYUNIT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_CURRENCYSUBUNIT =>
            Internal_Keyboard (SDLK_CURRENCYSUBUNIT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_LEFTPAREN =>
            Internal_Keyboard (SDLK_KP_LEFTPAREN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_RIGHTPAREN =>
            Internal_Keyboard (SDLK_KP_RIGHTPAREN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_LEFTBRACE =>
            Internal_Keyboard (SDLK_KP_LEFTBRACE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_RIGHTBRACE =>
            Internal_Keyboard (SDLK_KP_RIGHTBRACE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_TAB =>
            Internal_Keyboard (SDLK_KP_TAB) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_BACKSPACE =>
            Internal_Keyboard (SDLK_KP_BACKSPACE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_A =>
            Internal_Keyboard (SDLK_KP_A) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_B =>
            Internal_Keyboard (SDLK_KP_B) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_C =>
            Internal_Keyboard (SDLK_KP_C) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_D =>
            Internal_Keyboard (SDLK_KP_D) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_E =>
            Internal_Keyboard (SDLK_KP_E) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_F =>
            Internal_Keyboard (SDLK_KP_F) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_XOR =>
            Internal_Keyboard (SDLK_KP_XOR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_POWER =>
            Internal_Keyboard (SDLK_KP_POWER) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_PERCENT =>
            Internal_Keyboard (SDLK_KP_PERCENT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_LESS =>
            Internal_Keyboard (SDLK_KP_LESS) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_GREATER =>
            Internal_Keyboard (SDLK_KP_GREATER) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_AMPERSAND =>
            Internal_Keyboard (SDLK_KP_AMPERSAND) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_DBLAMPERSAND =>
            Internal_Keyboard (SDLK_KP_DBLAMPERSAND) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_VERTICALBAR =>
            Internal_Keyboard (SDLK_KP_VERTICALBAR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_DBLVERTICALBAR =>
            Internal_Keyboard (SDLK_KP_DBLVERTICALBAR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_COLON =>
            Internal_Keyboard (SDLK_KP_COLON) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_HASH =>
            Internal_Keyboard (SDLK_KP_HASH) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_SPACE =>
            Internal_Keyboard (SDLK_KP_SPACE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_AT =>
            Internal_Keyboard (SDLK_KP_AT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_EXCLAM =>
            Internal_Keyboard (SDLK_KP_EXCLAM) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_MEMSTORE =>
            Internal_Keyboard (SDLK_KP_MEMSTORE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_MEMRECALL =>
            Internal_Keyboard (SDLK_KP_MEMRECALL) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_MEMCLEAR =>
            Internal_Keyboard (SDLK_KP_MEMCLEAR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_MEMADD =>
            Internal_Keyboard (SDLK_KP_MEMADD) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_MEMSUBTRACT =>
            Internal_Keyboard (SDLK_KP_MEMSUBTRACT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_MEMMULTIPLY =>
            Internal_Keyboard (SDLK_KP_MEMMULTIPLY) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_MEMDIVIDE =>
            Internal_Keyboard (SDLK_KP_MEMDIVIDE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_PLUSMINUS =>
            Internal_Keyboard (SDLK_KP_PLUSMINUS) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_CLEAR =>
            Internal_Keyboard (SDLK_KP_CLEAR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_CLEARENTRY =>
            Internal_Keyboard (SDLK_KP_CLEARENTRY) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_BINARY =>
            Internal_Keyboard (SDLK_KP_BINARY) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_OCTAL =>
            Internal_Keyboard (SDLK_KP_OCTAL) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_DECIMAL =>
            Internal_Keyboard (SDLK_KP_DECIMAL) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KP_HEXADECIMAL =>
            Internal_Keyboard (SDLK_KP_HEXADECIMAL) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LCTRL =>
            Internal_Keyboard (SDLK_LCTRL) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LSHIFT =>
            Internal_Keyboard (SDLK_LSHIFT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LALT =>
            Internal_Keyboard (SDLK_LALT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_LGUI =>
            Internal_Keyboard (SDLK_LGUI) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_RCTRL =>
            Internal_Keyboard (SDLK_RCTRL) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_RSHIFT =>
            Internal_Keyboard (SDLK_RSHIFT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_RALT =>
            Internal_Keyboard (SDLK_RALT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_RGUI =>
            Internal_Keyboard (SDLK_RGUI) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_MODE =>
            Internal_Keyboard (SDLK_MODE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AUDIONEXT =>
            Internal_Keyboard (SDLK_AUDIONEXT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AUDIOPREV =>
            Internal_Keyboard (SDLK_AUDIOPREV) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AUDIOSTOP =>
            Internal_Keyboard (SDLK_AUDIOSTOP) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AUDIOPLAY =>
            Internal_Keyboard (SDLK_AUDIOPLAY) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AUDIOMUTE =>
            Internal_Keyboard (SDLK_AUDIOMUTE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_MEDIASELECT =>
            Internal_Keyboard (SDLK_MEDIASELECT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_WWW =>
            Internal_Keyboard (SDLK_WWW) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_MAIL =>
            Internal_Keyboard (SDLK_MAIL) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_CALCULATOR =>
            Internal_Keyboard (SDLK_CALCULATOR) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_COMPUTER =>
            Internal_Keyboard (SDLK_COMPUTER) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AC_SEARCH =>
            Internal_Keyboard (SDLK_AC_SEARCH) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AC_HOME =>
            Internal_Keyboard (SDLK_AC_HOME) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AC_BACK =>
            Internal_Keyboard (SDLK_AC_BACK) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AC_FORWARD =>
            Internal_Keyboard (SDLK_AC_FORWARD) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AC_STOP =>
            Internal_Keyboard (SDLK_AC_STOP) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AC_REFRESH =>
            Internal_Keyboard (SDLK_AC_REFRESH) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AC_BOOKMARKS =>
            Internal_Keyboard (SDLK_AC_BOOKMARKS) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_BRIGHTNESSDOWN =>
            Internal_Keyboard (SDLK_BRIGHTNESSDOWN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_BRIGHTNESSUP =>
            Internal_Keyboard (SDLK_BRIGHTNESSUP) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_DISPLAYSWITCH =>
            Internal_Keyboard (SDLK_DISPLAYSWITCH) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KBDILLUMTOGGLE =>
            Internal_Keyboard (SDLK_KBDILLUMTOGGLE) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KBDILLUMDOWN =>
            Internal_Keyboard (SDLK_KBDILLUMDOWN) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_KBDILLUMUP =>
            Internal_Keyboard (SDLK_KBDILLUMUP) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_EJECT =>
            Internal_Keyboard (SDLK_EJECT) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_SLEEP =>
            Internal_Keyboard (SDLK_SLEEP) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_APP1 =>
            Internal_Keyboard (SDLK_APP1) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_APP2 =>
            Internal_Keyboard (SDLK_APP2) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AUDIOREWIND =>
            Internal_Keyboard (SDLK_AUDIOREWIND) := (if Keys_Array.all = 0 then False else True);
         when SDL_SCANCODE_AUDIOFASTFORWARD =>
            Internal_Keyboard (SDLK_AUDIOFASTFORWARD) := (if Keys_Array.all = 0 then False else True);
         when SDL_NUM_SCANCODES =>
            Internal_Keyboard (SDLK_NUM_SCANCODES) := (if Keys_Array.all = 0 then False else True);
         when others =>
            null;
         end case;

         Ptr.Increment (Keys_Array);
      end loop;
   end Update_Keyboard_Status;

   procedure Swap_Buffers(Window : Window_ID; Erase : Boolean := True)is
   begin



      glFlush;
      if Stored_Windows(Window).window = null then
         Ada.Text_IO.Put_Line ("Error!!!");
      end if;
      SDL_GL_SwapWindow(Stored_Windows(Window).window);
--SDL_GL_SwapBuffers;
     -- SDL_Delay (1);
      glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  --    Poll_Events(S);


--        SDL_GL_SwapWindow(window);
--        if SDL_UpdateWindowSurface (Stored_Windows(Window).window) < 0 then
--           raise Display_Error;
--        end if;
--        if Erase then
--           if SDL_FillRect (Canvas.Surface, null, 0) < 0 then
--              raise Display_Error;
--           end if;
--        end if;
      Poll_Events;
      SDL_PumpEvents;
      Update_Keyboard_Status;

   end Swap_Buffers;
   --  Update the canvas

   -- copy the hidden buffer to the visible buffer
   procedure Swap_Copy_Buffers(Window : Window_ID)is null;
   --  Update the canvas

   function Get_Cursor_Status return Cursor_T is (Internal_Cursor);

   function Get_Key_Status (Key : Key_T) return Boolean is (Internal_Keyboard (Key));

   procedure Init is
   begin
      --  SDL is comprised of 8 subsystems. Here we initialize the video
      if SDL_Init(SDL_INIT_VIDEO) < 0 then
         Put_Line ("Error initializing SDL");
         Put_Line(Value (SDL_GetError));
         SDL_Quit;
      end if;



      Initialized := True;
   end Init;


begin

   Init;


end Display.Basic;
