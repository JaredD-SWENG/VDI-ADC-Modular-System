with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget;     use Gtk.Widget;
with Glib;           use Glib;
with Glib.Error;     use Glib.Error;
with Gdk.Event;
with Gtk.Tree_Model; use Gtk.Tree_Model;

with CV_Ada;

package GUI_Function is
   Builder  : Gtkada_Builder := null;

   type States is (On, Off);

   procedure Initialize (FilePath : String := "..\..\GladeGUI\CarSimulatorGUI.glade");

   procedure Preset_Images;

   procedure SetRawImage     (RawImage   : in out CV_Ada.Input_Data);
   procedure SetLeftImage    (LeftImage  : in out CV_Ada.Input_Data);
   procedure SetRightImage   (RightImage : in out CV_Ada.Input_Data);

   procedure AddConsoleText  (Text : String);

   procedure MainQuitClicked;

   task type Simulation_Wrapper is
      entry Start;
   end Simulation_Wrapper;

   Simulation_Task : Simulation_Wrapper;

   function  KeyPressed (Widget  : access Gtk_Widget_Record'Class; Event : Gdk.Event.Gdk_Event_Key) return Boolean;


   type Detection_Elements is (LeftSignal, RightSignal, RedLight, YellowLight, GreenLight);
   type Detection_Images_Array is array (Detection_Elements, States) of CV_Ada.Input_Data;

   procedure Set_Detection_Elements (Element : Detection_Elements; State : States);

private

   Error                  : aliased GError;

   Detection_Images : Detection_Images_Array;

   Console_Iterator  : Gtk_Tree_Iter := Null_Iter;

   Pixel_Array       : Guchar_Array_Access;
end GUI_Function;