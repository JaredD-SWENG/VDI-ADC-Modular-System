with Gdk.Event;
with Glib;           use Glib;
with Gtk.Image;      use Gtk.Image;
with Glib.Error;     use Glib.Error;
with Gtk.Widget;     use Gtk.Widget;
with Glib.Object;    use Glib.Object;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Tree_Model; use Gtk.Tree_Model;

with CV_Ada;

package GUI_Functions is
   Builder : Gtkada_Builder := null;

   function Does_Exist return Boolean;

   procedure Initialize      (FilePath : String := "glade_source\CarSimulator.glade");
   
   procedure AddConsoleText  (Text : String);
   
   -- This procedure is set this way so that when the user
   -- closes the GUI through ESC or the X button, it will
   -- call this procedure and quit the program.
   -- Do not modify.
   procedure MainQuitClicked (Builder : access Gtkada_Builder_Record'Class); -- Always keep

   type States is (On, Off);
   type Detection_Elements is (Left_Signal, Right_Signal, Red_Light, Yellow_Light, Green_Light);

   procedure Set_Detection_Elements (Element : Detection_Elements; State : States);

   protected Update_GUI is
      procedure Set_Raw_Image (Input_Data : CV_Ada.Input_Data);
      entry Get_Raw_Image (Input_Data : out CV_Ada.Input_Data);
   private
      Is_Set         : Boolean := False;
      Raw_Image_Data : CV_Ada.Input_Data;
   end Update_GUI;
private
   type Detection_Images_Array is array (Detection_Elements, States) of CV_Ada.Input_Data;

   Raw_Image         : CV_Ada.Input_Data;
   Exists            : Boolean                  := False;
   Quit_GUI          : Boolean                  := False;
   Error             : aliased GError;
   Detection_Images  : Detection_Images_Array;
   Console_Iterator  : Gtk_Tree_Iter            := Null_Iter;

   -- Function Stubs for Body

   function  GO (N : String) return GObject;

   procedure Preset_Images;

   procedure InitializeConsole;

   function  KeyPressed (Widget : access Gtk_Widget_Record'Class;
                         Event  : Gdk.Event.Gdk_Event_Key) return Boolean;

   procedure Update_Raw_Image;

   procedure Scale_QOI_Image_To_Window_Size (Image      : Gtk_Image;
                                             Input_Data : CV_Ada.Input_Data;
                                             Widget     : Gtk_Widget);

   function  Timeout_Check return Boolean;
end GUI_Functions;