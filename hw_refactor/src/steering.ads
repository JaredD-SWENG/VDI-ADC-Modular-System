with System_Config;

package Steering is

   procedure Init;
   function Get_Angle_Steering1 return Integer;
   procedure Set_Angle_Steering1 (A : Integer);

   private
   task Steering_Task with
      Storage_Size => 1 * 1024,
      Priority => System_Config.Steering_Priority;
end Steering;