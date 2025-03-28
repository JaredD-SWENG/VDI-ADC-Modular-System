with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget;     use Gtk.Widget;
with Glib;           use Glib;
with Glib.Error;     use Glib.Error;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Main;

with Gdk.Types;               use Gdk.Types;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Glib.Object;             use Glib.Object;
with Gtk.List_Store;          use Gtk.List_Store;
with Gtk.Window;              use Gtk.Window;
with Gtk.Enums;               use Gtk.Enums;
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Gtk.Image;               use Gtk.Image;
with Gdk.Event;               use Gdk.Event;
with Glib.Main;               use Glib.Main;
with Glib.Types;              use Glib.Types;

with Ada.Text_IO;             use Ada.Text_IO;

with Ada.Strings.Fixed;
with Ada.Strings;

with Acv;

with System;

with Ada.Unchecked_Conversion;

with Ada.Containers.Indefinite_Vectors;

package body Video is

   Builder  : Gtkada_Builder := null;

   package Frames is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Natural,
      Element_Type => Acv.Image_T,
      "="          => Acv."=");

   subtype Video is Frames.Vector;

   function Zero_Pad (N : Natural; Width : Natural) return String is
      Clean : constant String := Ada.Strings.Fixed.Trim (N'Image, Ada.Strings.Left); -- remove leading space
      Result : constant String := Ada.Strings.Fixed.Tail (Clean, Width, '0');
   begin
      return Result;
   end Zero_Pad;

   function Load_Video (Dir : String) return Video is
      Nbr_Frames : Natural := Acv.Imgs_Count_In_Dir (Dir);
      Vid : Video;
   begin 
      for I in 0 .. Nbr_Frames - 1 loop
         Vid.Append (Acv.From_Qoi (Dir, "frame_" & Zero_Pad (I, 4)));
      end loop;
      return Vid;
   end Load_Video;

   Vid : Video := Load_Video ("../camera/frames_folder");

   procedure Init (B : Gtkada_Builder) is
   begin
      Builder := B;
   end Init;
   procedure Set_Image_To_Object  (Img : Acv.Image_T; Object_Name : String) is
      Obj      : constant GObject    := Get_Object (Builder, Object_Name);
      Image    : constant Gtk_Image  := Gtk_Image (Obj);
      Widget   : constant Gtk_Widget := Gtk_Widget (Obj);
      Height   : constant Gint       := Get_Allocated_Height (Widget);
      Width    : constant Gint       := Get_Allocated_Width (Widget);
      Img_Data : Guchar_Array (0 .. (Img'Length(1) * Img'Length(2) * 3) - 1)
         with Address => Img'Address;
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Guchar_Array_Access);
      
      Pixbuf : constant Gdk_Pixbuf := Gdk_New_From_Data (Data      => Convert(Img_Data'Address), 
                                                         Width     => Gint (Img'Length(1)), 
                                                         Height    => Gint (Img'Length(2)), 
                                                         Rowstride => Gint (Img'Length(1) * 3));
   begin
      Set (Image, Scale_Simple (Pixbuf, Width, Height, Interp_Nearest));
   end Set_Image_To_Object;

   Cursor : Frames.Cursor := Vid.First;
   function Frame_Callback return Boolean is
      use Frames;
      Current_Frame : constant Acv.Image_T := Frames.Element (Cursor);
      --Current_Gray  : constant Acv.Image_T := Acv.Gray (Current_Frame);
      Current_BW    : constant Acv.Image_T := Acv.Black_And_White (Current_Frame, 128);
   begin
      Set_Image_To_Object (Current_Frame, "LeftImage");
      Set_Image_To_Object (Current_BW, "RightImage");
      Cursor := (if Cursor = Vid.Last then Vid.First else Frames.Next (Cursor));
      return True;
   end Frame_Callback;

end Video;