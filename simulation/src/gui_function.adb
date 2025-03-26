with Gtk.Main;
with Gdk.Threads;
with Gtk.Builder;             use Gtk.Builder;
with Gdk.Types;               use Gdk.Types;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Glib.Object;             use Glib.Object;
with Gtk.List_Store;          use Gtk.List_Store;
with Gtk.Window;              use Gtk.Window;
with Gtk.Enums;               use Gtk.Enums;
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Gtk.Image;               use Gtk.Image;

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Exceptions;
with System.Storage_Elements; use System.Storage_Elements;

with CV_Ada.IO_Operations;

package body GUI_Function is
   function Set_QOI_To_Gdk_Pixbuf(InputData : CV_Ada.Input_Data) return Gdk_Pixbuf is
      Index    : Storage_Offset := 1;
   begin
      if Pixel_Array = null or else Pixel_Array'Length /= InputData.Desc.Width * InputData.Desc.Height * InputData.Desc.Channels then
         Pixel_Array := new Guchar_Array(1 .. Integer(InputData.Desc.Width * InputData.Desc.Height * InputData.Desc.Channels)); -- Memory leak?
      end if;

      while Index <= Pixel_Array'Length
      loop
         Pixel_Array (Integer(Index)) := Guchar(InputData.Data(Index));
         Index := @ + 1;
      end loop;

      return Gdk_New_From_Data(Data => Pixel_Array, Width => Gint(InputData.Desc.Width), Height => Gint(InputData.Desc.Height), Rowstride => Gint(InputData.Desc.Channels * InputData.Desc.Width));
   end;

   procedure Scale_QOI_Image_To_Window_Size(Image : Gtk_Image; InputData : CV_Ada.Input_Data; Widget : Gtk_Widget; Width : Gint := 0; Height : Gint := 0) is
   begin
      Set (Image, Scale_Simple(Set_QOI_To_Gdk_Pixbuf(InputData), Get_Allocated_Width(Widget)+Width, Get_Allocated_Height(Widget)+Height, Interp_Nearest));
   end;

   task body Simulation_Wrapper is
   begin
      loop
         select
            accept Start do
               Initialize;
            end Start;
         or terminate;
         end select;

         --  declare
         --     UnusedBoolean : Boolean;
         --  begin
         --     while Gtk.Main.Events_Pending loop
         --        UnusedBoolean := Gtk.Main.Main_Iteration;
         --     end loop;
         --  end;
      end loop;

      --  loop
      --     select
      --        accept SetRawImage (RawImage : in out CV_Ada.Input_Data) do
      --           SetRawImage(RawImage);
      --        end SetRawImage;
      --     or
      --        accept SetLeftImage  (LeftImage  : in out CV_Ada.Input_Data) do
      --           SetLeftImage(LeftImage);
      --        end SetLeftImage;
      --     or
      --        accept SetRightImage (RightImage : in out CV_Ada.Input_Data) do
      --           SetRightImage(RightImage);
      --        end SetRightImage;
      --     or
      --        accept RightSignalOn do
      --           SetRightSignal(On);
      --        end RightSignalOn;
      --     or
      --        accept RightSignalOff do
      --           SetRightSignal(Off);
      --        end RightSignalOff;
      --     or
      --        accept LeftSignalOn do
      --           SetLeftSignal(On);
      --        end LeftSignalOn;
      --     or
      --        accept LeftSignalOff do
      --           SetLeftSignal(Off);
      --        end LeftSignalOff;
      --     or
      --        accept RedLightOn do
      --           SetRedLight(On);
      --        end RedLightOn;
      --     or
      --        accept RedLightOff do
      --           SetRedLight(Off);
      --        end RedLightOff;
      --     or
      --        accept YellowLightOn do
      --           SetYellowLight(On);
      --        end YellowLightOn;
      --     or
      --        accept YellowLightOff do
      --           SetYellowLight(Off);
      --        end YellowLightOff;
      --     or
      --        accept GreenLightOn do
      --           SetGreenLight(On);
      --        end GreenLightOn;
      --     or
      --        accept GreenLightOff do
      --           SetGreenLight(Off);
      --        end GreenLightOff;
      --     or
      --        accept AddConsoleText (Text : String) do
      --           AddConsoleText(Text);
      --        end AddConsoleText;
      --     or
      --        accept MainQuitClicked do
      --           Gtk.Main.Main_Quit;
      --        end MainQuitClicked;
      --     or terminate;
      --     end select;
      --  end loop;
   end Simulation_Wrapper;

   procedure InitializeConsole is
      ConsoleObject : constant GObject := Get_Object (Builder, Name => "listStore");
   begin
      Clear (Gtk_List_Store(ConsoleObject));

      if Console_Iterator = Null_Iter then
         Console_Iterator := Get_Iter_First (Gtk_List_Store(ConsoleObject));
      end if;
   end InitializeConsole;

   procedure Initialize (FilePath : String := "..\..\GladeGUI\CarSimulatorGUI.glade") is
   begin
      --  Gdk.Threads.G_Init;
      --  Gdk.Threads.Init;
      Gtk.Main.Init;
      Gtk_New (Builder);

      if Add_From_File (Gtk_Builder (Builder), FilePath, Error'Access) = 0 then
         Put_Line ("Error: " & Get_Message (Error));
         Error_Free (Error);
         return;
      end if;

      Put_Line ("Builder Loading OK: " & FilePath);

      On_Key_Press_Event(Gtk_Widget (Get_Object (Builder, "topWindow")), GUI_Function.KeyPressed'Access);
      On_Key_Press_Event(Gtk_Widget (Get_Object (Builder, "rawWindow")), GUI_Function.KeyPressed'Access);

      Do_Connect (Builder);

      Gtk.Window.Set_Position (Gtk_Window (Get_Object (Builder, "topWindow")), Win_Pos_Center);
      Gtk.Window.Set_Position (Gtk_Window (Get_Object (Builder, "rawWindow")), Win_Pos_Center);

      Show_All (Gtk_Widget (Get_Object (Gtk_Builder(Builder), "topWindow")));
      Show_All (Gtk_Widget (Get_Object (Gtk_Builder(Builder), "rawWindow")));

      --  Gdk.Threads.Enter;

      Preset_Images;
      InitializeConsole;
      AddConsoleText ("Simulation GUI Started" & ASCII.LF & ASCII.LF & "Press 'ESC' to Quit");

      Gtk.Main.Main;
      --  Gdk.Threads.Leave;

      Unref (Builder); -- Free memory because I was told to do so
   exception
      when Error : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Initialize;

   procedure Preset_Images is
      BasePath          : constant String    := "..\simulation_gui\images\";
      LeftSignalObject  : constant GObject   := Get_Object (Builder, Name => "leftSignal");
      RightSignalObject : constant GObject   := Get_Object (Builder, Name => "rightSignal");
      RedLightObject    : constant GObject   := Get_Object (Builder, Name => "redLight");
      YellowLightObject : constant GObject   := Get_Object (Builder, Name => "yellowLight");
      GreenLightObject  : constant GObject   := Get_Object (Builder, Name => "greenLight");
   begin
      LeftSignalOffImageRef  := CV_Ada.IO_Operations.Load_QOI(BasePath & "LeftSignalEmpty.qoi");
      LeftSignalOnImageRef   := CV_Ada.IO_Operations.Load_QOI(BasePath & "LeftSignalFilled.qoi");
      RightSignalOffImageRef := CV_Ada.IO_Operations.Load_QOI(BasePath & "RightSignalEmpty.qoi");
      RightSignalOnImageRef  := CV_Ada.IO_Operations.Load_QOI(BasePath & "RightSignalFilled.qoi");
      RedLightOffImageRef    := CV_Ada.IO_Operations.Load_QOI(BasePath & "RedLightEmpty.qoi");
      RedLightOnImageRef     := CV_Ada.IO_Operations.Load_QOI(BasePath & "RedLightFilled.qoi");
      YellowLightOffImageRef := CV_Ada.IO_Operations.Load_QOI(BasePath & "YellowLightEmpty.qoi");
      YellowLightOnImageRef  := CV_Ada.IO_Operations.Load_QOI(BasePath & "YellowLightFilled.qoi");
      GreenLightOffImageRef  := CV_Ada.IO_Operations.Load_QOI(BasePath & "GreenLightEmpty.qoi");
      GreenLightOnImageRef   := CV_Ada.IO_Operations.Load_QOI(BasePath & "GreenLightFilled.qoi");

      Scale_QOI_Image_To_Window_Size(Gtk_Image(LeftSignalObject),  LeftSignalOffImageRef,  Gtk_Widget(LeftSignalObject));
      Scale_QOI_Image_To_Window_Size(Gtk_Image(RightSignalObject), RightSignalOffImageRef, Gtk_Widget(RightSignalObject));
      Scale_QOI_Image_To_Window_Size(Gtk_Image(RedLightObject),    RedLightOffImageRef,    Gtk_Widget(RedLightObject));
      Scale_QOI_Image_To_Window_Size(Gtk_Image(YellowLightObject), YellowLightOffImageRef, Gtk_Widget(YellowLightObject));
      Scale_QOI_Image_To_Window_Size(Gtk_Image(GreenLightObject),  GreenLightOffImageRef,  Gtk_Widget(GreenLightObject));
   end;

   procedure AddConsoleText (Text : String) is
      ConsoleObject : constant GObject := Get_Object (Builder, Name => "listStore");
   begin
      Append (Gtk_List_Store(ConsoleObject), Console_Iterator);
      Set (Gtk_List_Store(ConsoleObject), Console_Iterator, 0, Text);
   end AddConsoleText;

   procedure SetRawImage (RawImage : in out CV_Ada.Input_Data) is
      RawImageObject : constant GObject := Get_Object (Builder, Name => "rawImage");
   begin
      Scale_QOI_Image_To_Window_Size(Gtk_Image(RawImageObject), RawImage, Gtk_Widget(RawImageObject));
   end SetRawImage;

   procedure SetLeftImage  (LeftImage  : in out CV_Ada.Input_Data) is
      LeftImageObject : constant GObject := Get_Object (Builder, Name => "leftImage");
   begin
      Scale_QOI_Image_To_Window_Size(Gtk_Image(LeftImageObject), LeftImage, Gtk_Widget(LeftImageObject));
   end SetLeftImage;

   procedure SetRightImage (RightImage : in out CV_Ada.Input_Data) is
      RightImageObject : constant GObject := Get_Object (Builder, Name => "rightImage");
   begin
      Scale_QOI_Image_To_Window_Size(Gtk_Image(RightImageObject), RightImage, Gtk_Widget(RightImageObject));
   end SetRightImage;

   procedure RightSignalOn is
   begin
      SetRightSignal(On);
   end RightSignalOn;

   procedure RightSignalOff is
   begin
      SetRightSignal(Off);
   end RightSignalOff;

   procedure LeftSignalOn is
   begin
      SetLeftSignal(On);
   end LeftSignalOn;

   procedure LeftSignalOff is
   begin
      SetLeftSignal(Off);
   end LeftSignalOff;

   procedure RedLightOn is
   begin
      SetRedLight(On);
   end RedLightOn;

   procedure RedLightOff is
   begin
      SetRedLight(Off);
   end RedLightOff;

   procedure YellowLightOn is
   begin
      SetYellowLight(On);
   end YellowLightOn;

   procedure YellowLightOff is
   begin
      SetYellowLight(Off);
   end YellowLightOff;

   procedure GreenLightOn is
   begin
      SetGreenLight(On);
   end GreenLightOn;

   procedure GreenLightOff is
   begin
      SetGreenLight(Off);
   end GreenLightOff;

   procedure MainQuitClicked is
   begin
      Gtk.Main.Main_Quit;
   end MainQuitClicked;

   function  KeyPressed (Widget : access Gtk_Widget_Record'Class; Event : Gdk.Event.Gdk_Event_Key) return Boolean is
      pragma Unreferenced (Widget);
   begin
      -- Could use P for pausing the simulation/camera
      if Event.Keyval = GDK_Escape then Gtk.Main.Main_Quit; end if;
      return True;
      --  if Event.Keyval = GDK_Escape then MainQuitClicked(null); end if;
      --  return True;
   end;

   procedure SetLeftSignal (State : States) is
      LeftSignalObject  : constant GObject   := Get_Object (Builder, Name => "leftSignal");
   begin
      if State = On then
         Scale_QOI_Image_To_Window_Size(Gtk_Image(LeftSignalObject), LeftSignalOnImageRef, Gtk_Widget(LeftSignalObject));
      else
         Scale_QOI_Image_To_Window_Size(Gtk_Image(LeftSignalObject), LeftSignalOffImageRef, Gtk_Widget(LeftSignalObject));
      end if;
   end;
   
   procedure SetRightSignal (State : States) is
      RightSignalObject : constant GObject   := Get_Object (Builder, Name => "rightSignal");
   begin
      if State = On then
         Scale_QOI_Image_To_Window_Size(Gtk_Image(RightSignalObject), RightSignalOnImageRef, Gtk_Widget(RightSignalObject));
      else
         Scale_QOI_Image_To_Window_Size(Gtk_Image(RightSignalObject), RightSignalOffImageRef, Gtk_Widget(RightSignalObject));
      end if;
   end;
   
   procedure SetRedLight (State : States) is
      RedLightObject    : constant GObject   := Get_Object (Builder, Name => "redLight");
   begin
      if State = On then
         Scale_QOI_Image_To_Window_Size(Gtk_Image(RedLightObject), RedLightOnImageRef, Gtk_Widget(RedLightObject));
      else
         Scale_QOI_Image_To_Window_Size(Gtk_Image(RedLightObject), RedLightOffImageRef, Gtk_Widget(RedLightObject));
      end if;
   end;
   
   procedure SetYellowLight (State : States) is
      YellowLightObject : constant GObject   := Get_Object (Builder, Name => "yellowLight");
   begin
      if State = On then
         Scale_QOI_Image_To_Window_Size(Gtk_Image(YellowLightObject), YellowLightOnImageRef, Gtk_Widget(YellowLightObject));
      else
         Scale_QOI_Image_To_Window_Size(Gtk_Image(YellowLightObject), YellowLightOffImageRef, Gtk_Widget(YellowLightObject));
      end if;
   end;
   
   procedure SetGreenLight (State : States) is
      GreenLightObject  : constant GObject   := Get_Object (Builder, Name => "greenLight");
   begin
      if State = On then
         Scale_QOI_Image_To_Window_Size(Gtk_Image(GreenLightObject), GreenLightOnImageRef, Gtk_Widget(GreenLightObject));
      else
         Scale_QOI_Image_To_Window_Size(Gtk_Image(GreenLightObject), GreenLightOffImageRef, Gtk_Widget(GreenLightObject));
      end if;
   end;
end GUI_Function;