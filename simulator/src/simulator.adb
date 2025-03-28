with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget;     use Gtk.Widget;
with Glib;           use Glib;
with Glib.Error;     use Glib.Error;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Main;
with Gtk.Builder;             use Gtk.Builder;
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

with Acv;
with Video;

with System;

with Ada.Unchecked_Conversion;

procedure Simulator is
   Builder  : Gtkada_Builder := null;
   Glade_File : String := "CarSimulatorGUI.glade";
   Error             : aliased GError;
   ID : G_Source_Id;
   Frame_Interval : constant Glib.Guint := 1000 / 30;
begin
   Gtk.Main.Init;
   Gtk_New (Builder);
   Video.Init (Builder);

   if Add_From_File (Gtk_Builder (Builder), Glade_File, Error'Access) = 0 then
         Put_Line ("Error: " & Get_Message (Error));
         Error_Free (Error);
         return;
   end if;
   Do_Connect (Builder);

   ID := Glib.Main.Timeout_Add (Interval => Frame_Interval, 
                                Func     => Video.Frame_Callback'Access);

   Show_All (Gtk_Widget (Get_Object (Gtk_Builder(Builder), "topWindow")));

   

   --  declare
   --     Gray : Acv.Image_T := Acv.Gray (Img);
   --     BW  : Acv.Image_T := Acv.Black_And_White (Img, 128);
   --  begin
   --     Set_Image_To_Object (Gray, "LeftImage");
   --     Set_Image_To_Object (BW, "RightImage");
   --  end;
   Gtk.Main.Main;
end Simulator;
