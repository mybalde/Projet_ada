package Display.Image is

   function Load_BMP (File : String) return Image_T;

   procedure Free (X : in out Image_T);

end Display.Image;
