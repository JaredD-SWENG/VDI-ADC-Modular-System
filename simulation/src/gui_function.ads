with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget;     use Gtk.Widget;
with Glib;           use Glib;
with Glib.Error;     use Glib.Error;
with Gdk.Event;
with Gtk.Tree_Model; use Gtk.Tree_Model;

with CV_Ada;

package GUI_Function is
   Builder  : Gtkada_Builder := null;

   type States is (On, Off, None);

   procedure Initialize (FilePath : String := "..\..\GladeGUI\CarSimulatorGUI.glade");

   procedure Preset_Images;

   procedure SetRawImage     (RawImage   : in out CV_Ada.Input_Data);
   procedure SetLeftImage    (LeftImage  : in out CV_Ada.Input_Data);
   procedure SetRightImage   (RightImage : in out CV_Ada.Input_Data);
   procedure RightSignalOn;
   procedure RightSignalOff;
   procedure LeftSignalOn;
   procedure LeftSignalOff;
   procedure RedLightOn;
   procedure RedLightOff;
   procedure YellowLightOn;
   procedure YellowLightOff;
   procedure GreenLightOn;
   procedure GreenLightOff;

   procedure AddConsoleText  (Text : String);

   procedure MainQuitClicked;

   task type Simulation_Wrapper is
      -- Don't touch or I'll touch you
      entry Start;
      --  entry SetRawImage     (RawImage   : in out CV_Ada.Input_Data);
      --  entry SetLeftImage    (LeftImage  : in out CV_Ada.Input_Data);
      --  entry SetRightImage   (RightImage : in out CV_Ada.Input_Data);
      --  entry RightSignalOn;
      --  entry RightSignalOff;
      --  entry LeftSignalOn;
      --  entry LeftSignalOff;
      --  entry RedLightOn;
      --  entry RedLightOff;
      --  entry YellowLightOn;
      --  entry YellowLightOff;
      --  entry GreenLightOn;
      --  entry GreenLightOff;

      --  entry AddConsoleText  (Text : String);

      --  entry MainQuitClicked; -- Always keep
   end Simulation_Wrapper;

   Simulation_Task : Simulation_Wrapper;

   function  KeyPressed          (Widget  : access Gtk_Widget_Record'Class; Event : Gdk.Event.Gdk_Event_Key) return Boolean;
private
   --  procedure Preset_Images;
   --  procedure InitializeConsole;
   --  procedure AddConsoleText      (Text : String);
   --  procedure SetRawImage         (RawImage   : in out CV_Ada.Input_Data);
   --  procedure SetLeftImage        (LeftImage  : in out CV_Ada.Input_Data);
   --  procedure SetRightImage       (RightImage : in out CV_Ada.Input_Data);

   Error                  : aliased GError;

   LeftSignalOffImageRef  : CV_Ada.Input_Data;
   LeftSignalOnImageRef   : CV_Ada.Input_Data;
   RightSignalOffImageRef : CV_Ada.Input_Data;
   RightSignalOnImageRef  : CV_Ada.Input_Data;
   RedLightOffImageRef    : CV_Ada.Input_Data;
   RedLightOnImageRef     : CV_Ada.Input_Data;
   YellowLightOffImageRef : CV_Ada.Input_Data;
   YellowLightOnImageRef  : CV_Ada.Input_Data;
   GreenLightOffImageRef  : CV_Ada.Input_Data;
   GreenLightOnImageRef   : CV_Ada.Input_Data;

   procedure SetRightSignal (State : States);
   procedure SetLeftSignal  (State : States);
   procedure SetRedLight    (State : States);
   procedure SetYellowLight (State : States);
   procedure SetGreenLight  (State : States);

   Console_Iterator  : Gtk_Tree_Iter := Null_Iter;

   Pixel_Array       : Guchar_Array_Access;
end GUI_Function;