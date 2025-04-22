with Gtk.Main;
with Gdk.Threads;
with Glib.Main;         use Glib.Main;
with Gtk.Enums;         use Gtk.Enums;
with Gdk.Types;         use Gdk.Types;
with Gtk.Window;        use Gtk.Window;
with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Gtk.Builder;       use Gtk.Builder;
with Gtk.List_Store;    use Gtk.List_Store;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;

with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;              use Ada.Text_IO;
with System.Storage_Elements;  use System.Storage_Elements;

with GNAT.OS_Lib;

with CV_Ada.IO_Operations;
use CV_Ada;

with Camera;

package body GUI_Functions is
   function GO (N : String) return GObject is (Get_Object(Builder, Name => N));

   procedure Initialize (FilePath : String := "glade_source\CarSimulator.glade") is
      Timeout : G_Source_Id;
   begin
      Gdk.Threads.G_Init;
      Gdk.Threads.Init;
      Gtk.Main.Init;
      Gtk_New (Builder);

      if Add_From_File (Gtk_Builder (Builder), FilePath, Error'Access) = 0 then
         Put_Line ("Error: " & Get_Message (Error));
         Error_Free (Error);
         return;
      end if;

      Put_Line ("Builder Loading OK: " & FilePath);

      Register_Handler
        (Builder      => Builder,
         Handler_Name => "MainQuit",
         Handler      => GUI_Functions.MainQuitClicked'Access);

      On_Key_Press_Event(Gtk_Widget (GO ("topWindow")), KeyPressed'Access);

      Do_Connect (Builder);

      Gtk.Window.Set_Position (Gtk_Window (GO ("topWindow")), Win_Pos_Center);

      Show_All (Gtk_Widget (GO ("topWindow")));

      Preset_Images;
      InitializeConsole;
      AddConsoleText ("Simulation GUI Started" & ASCII.LF & ASCII.LF & "Press 'ESC' to Quit");

      Exists := True;

      Timeout := Timeout_Add (50, Timeout_Check'Access);
      --  Timeout := Idle_Add (Timeout_Check'Access);

      Gdk.Threads.Enter;
      Gtk.Main.Main;
      Gdk.Threads.Leave;

      Unref (Builder); -- Free memory because I was told to do so
   exception
      when Error : others =>
         Put_Line ("Error: Initialization failed");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
         GNAT.OS_Lib.OS_Exit (1);
   end Initialize;

   function Does_Exist return Boolean is (Exists);

   procedure Preset_Images is
      BasePath : constant String := "..\simulation_gui\images\";
   begin
      for Element in Detection_Elements'Range loop
         for State in States'Range loop
            Detection_Images (Element, State) := CV_Ada.IO_Operations.Load_QOI(BasePath & Element'Image & "_" & State'Image & ".qoi");
         end loop;
         Scale_QOI_Image_To_Window_Size(Gtk_Image(GO (Element'Image)),  
                                        Detection_Images(Element, Off),  
                                        Gtk_Widget(GO (Element'Image)));
      end loop;
   end;

   procedure InitializeConsole is
      ConsoleObject : constant GObject := Get_Object (Builder, Name => "listStore");
   begin
      Clear (Gtk_List_Store(ConsoleObject));

      if Console_Iterator = Null_Iter then
         Console_Iterator := Get_Iter_First (Gtk_List_Store(ConsoleObject));
      end if;
   end InitializeConsole;

   procedure AddConsoleText (Text : String) is
      ConsoleObject : GObject;
   begin
      if Does_Exist = False then
         return;
      end if;
      Gdk.Threads.Enter;
      ConsoleObject := Get_Object (Builder, Name => "listStore");
      Append (Gtk_List_Store(ConsoleObject), Console_Iterator);
      Set (Gtk_List_Store(ConsoleObject), Console_Iterator, 0, Text);
      Gdk.Threads.Leave;
   end AddConsoleText;

   procedure MainQuitClicked (Builder : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Builder);
   begin
      Exists := False;
      --  Gdk.Threads.Enter;
      Gtk.Main.Main_Quit;
      --  Gdk.Threads.Leave;
      delay 1.0;
   end MainQuitClicked;

   function KeyPressed (Widget : access Gtk_Widget_Record'Class; Event : Gdk.Event.Gdk_Event_Key) return Boolean is
      pragma Unreferenced (Widget);
   begin
      -- Could use P for pausing the simulation/camera
      if Event.Keyval = GDK_Escape then MainQuitClicked(null); end if;
      return True;
   end;

   protected body Update_GUI is
      procedure Set_Raw_Image (Input_Data : CV_Ada.Input_Data) is
      begin
         Raw_Image_Data := Input_Data;
         Is_Set := True;
      end Set_Raw_Image;

      entry Get_Raw_Image (Input_Data : out CV_Ada.Input_Data)
         when Is_Set is
      begin
         Input_Data := Raw_Image_Data;
         --  Is_Set := False;
      end Get_Raw_Image;
   end Update_GUI;

   procedure Update_Raw_Image is
      Object    : constant GObject := Get_Object (Builder, Name => "RawImage");
      Raw_Image : CV_Ada.Input_Data;
   begin
      --  Gdk.Threads.Enter;
      Raw_Image := Camera.Vid (Camera.Get_Current_Global_Frame);
      --  Raw_Image := CV_Ada.IO_Operations.Load_QOI (Camera.Get_Next_Frame_Path ("Lane_Detection"));
      --  Update_GUI.Get_Raw_Image (Raw_Image);
      if Raw_Image.Data /= null then
         --  Put_Line (Camera.Get_Current_Global_Frame'Image);
         --  Put_Line (Raw_Image.Data.all'Length'Image);
         Scale_QOI_Image_To_Window_Size (Gtk_Image (Object), Raw_Image, Gtk_Widget (Object));
      end if;
      --  Gdk.Threads.Leave;
   exception
      when E : others =>
         Put_Line ("Error: Update_Raw_Image failed");
         Put_Line ("Error: " & Ada.Exceptions.Exception_Information (E));
         GNAT.OS_Lib.OS_Exit (1);
   end Update_Raw_Image;

   procedure Set_Detection_Elements (Element : Detection_Elements; State : States) is
      Object : GObject;
   begin
      if Does_Exist = False then
         return;
      end if;
      --  Gdk.Threads.Enter;
      Object := Get_Object (Builder, Name => Element'Image);
      Scale_QOI_Image_To_Window_Size (Gtk_Image (Object), Detection_Images (Element, State), Gtk_Widget (Object));
      --  Gdk.Threads.Leave;
   end Set_Detection_Elements;

   procedure Scale_QOI_Image_To_Window_Size(Image : Gtk_Image; Input_Data : CV_Ada.Input_Data; Widget : Gtk_Widget) is
      Pixel_Array : Guchar_Array (0 .. Integer(Input_Data.Desc.Width * Input_Data.Desc.Height * Input_Data.Desc.Channels) - 1)
         with Address => Input_Data.Data.all'Address;
      function Convert is new Ada.Unchecked_Conversion (System.Address, Guchar_Array_Access);
      
      Pixbuf : constant Gdk_Pixbuf := Gdk_New_From_Data(Data      => Convert (Pixel_Array'Address),
                                                        Width     => Gint(Input_Data.Desc.Width),
                                                        Height    => Gint(Input_Data.Desc.Height),
                                                        Rowstride => Gint(Input_Data.Desc.Channels * Input_Data.Desc.Width));
   begin
      Gdk.Threads.Enter;
      Set (Image, Scale_Simple(Pixbuf, Get_Allocated_Width(Widget), Get_Allocated_Height(Widget), Interp_Nearest));
      Gdk.Threads.Leave;
   exception
      when E : others =>
         Put_Line ("Error: Scale_QOI_Image_To_Window_Size failed");
         Put_Line (Ada.Exceptions.Exception_Information (E));
         GNAT.OS_Lib.OS_Exit (1);
   end;

   function Timeout_Check return Boolean is
   begin
      Update_Raw_Image;
      return True;
   exception
      when E : others => 
      Put_Line ("Error: Timeout_Check failed");
      Put_Line (Ada.Exceptions.Exception_Information (E));
      GNAT.OS_Lib.OS_Exit (1);
   end Timeout_Check;

end GUI_Functions;