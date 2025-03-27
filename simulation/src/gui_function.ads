with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget;     use Gtk.Widget;
with Glib;           use Glib;
with Glib.Error;     use Glib.Error;
with Gtk.Tree_Model; use Gtk.Tree_Model;

with CV_Ada;

package GUI_Function is
   Builder  : Gtkada_Builder := null;

   type States is (On, Off);

   procedure Initialize (FilePath : String := "..\simulation_gui\glade_source\CarSimulatorGUI.glade");

   procedure SetRawImage     (RawImage   : in out CV_Ada.Input_Data);
   procedure SetLeftImage    (LeftImage  : in out CV_Ada.Input_Data);
   procedure SetRightImage   (RightImage : in out CV_Ada.Input_Data);

   procedure AddConsoleText  (Text : String);

   task type Simulation_Wrapper is
      entry Start;
   end Simulation_Wrapper;

   Simulation_Task : Simulation_Wrapper;

   type Detection_Elements is (Left_Signal, Right_Signal, Red_Light, Yellow_Light, Green_Light);
   type Detection_Images_Array is array (Detection_Elements, States) of CV_Ada.Input_Data;

   procedure Set_Detection_Elements (Element : Detection_Elements; State : States);

private
   Error             : aliased GError;
   Detection_Images  : Detection_Images_Array;
   Console_Iterator  : Gtk_Tree_Iter := Null_Iter;
   Pixel_Array       : Guchar_Array_Access;
end GUI_Function;