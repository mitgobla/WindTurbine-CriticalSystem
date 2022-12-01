pragma SPARK_Mode (On);

with AS_Io_Wrapper; use AS_Io_Wrapper;

package body Wind_Turbine is

   -- Take in the input of the wind speed detected by the wind turbine sensors
   procedure Read_Wind_Speed is
      Speed : Integer;
   begin
      AS_Put_Line
        ("Please enter the wind speed (mph) being read by the wind turbine sensors");
      loop
         AS_Get (Speed, "Please type in an integer.");
         exit when (Speed >= 0) and (Speed <= Maximum_Wind_Speed);
         AS_Put ("Please type in a value between 0 and ");
         AS_Put (Maximum_Wind_Speed);
         AS_Put_Line ("");
      end loop;
      Status_Turbine.Speed_Measured := Speed_Range (Speed);
   end Read_Wind_Speed;

   -- Convert the Tip Brake status to a string
   function Status_Tip_Brake_To_String
     (Status_Tip_Brake : Status_Tip_Brake_Type) return String
   is
   begin
      case Status_Tip_Brake is
         when Activated =>
            return "Activated";
         when Deactivated =>
            return "Deactivated";
      end case;
   end Status_Tip_Brake_To_String;

   -- Conver the mechanical brake status to a string
   function Status_Mechanical_Brake_To_String
     (Status_Mechanical_Brake : Status_Mechanical_Brake_Type) return String
   is
   begin
      case Status_Mechanical_Brake is
         when Activated =>
            return "Activated";
         when Deactivated =>
            return "Deactivated";
      end case;
   end Status_Mechanical_Brake_To_String;

   procedure Print_Status is
   begin
      AS_Put ("Wind speed (mph) = ");
      AS_Put (Integer (Status_Turbine.Speed_Measured));
      AS_Put_Line ("");
      AS_Put ("Tip Brake = ");
      AS_Put_Line
        (Status_Tip_Brake_To_String (Status_Turbine.Status_Tip_Brake));
      AS_Put ("Mechanical Brake = ");
      AS_Put_Line
        (Status_Mechanical_Brake_To_String
           (Status_Turbine.Status_Mechanical_Brake));
   end Print_Status;

   procedure Monitor_Turbine is
      Speed : Integer;
   begin
      Speed := Integer (Status_Turbine.Speed_Measured);
      if (Speed in 0 .. Startup_Wind_Speed) then
         Status_Turbine.Status_Tip_Brake        := Activated;
         Status_Turbine.Status_Mechanical_Brake := Activated;
      elsif (Speed in Startup_Wind_Speed .. Shutdown_Wind_Speed) then
         Status_Turbine.Status_Tip_Brake        := Deactivated;
         Status_Turbine.Status_Mechanical_Brake := Deactivated;
      elsif (Speed in Shutdown_Wind_Speed .. Critical_Wind_Speed) then
         Status_Turbine.Status_Tip_Brake        := Activated;
         Status_Turbine.Status_Mechanical_Brake := Deactivated;
      elsif (Speed in Critical_Wind_Speed .. Maximum_Wind_Speed) then
         Status_Turbine.Status_Tip_Brake        := Activated;
         Status_Turbine.Status_Mechanical_Brake := Activated;
      else
         Status_Turbine.Status_Tip_Brake        := Activated;
         Status_Turbine.Status_Mechanical_Brake := Activated;
      end if;
   end Monitor_Turbine;

   procedure Initialize is
   begin
      AS_Init_Standard_Input;
      AS_Init_Standard_Output;
      Status_Turbine :=
        (Speed_Measured          => 0, Status_Tip_Brake => Activated,
         Status_Mechanical_Brake => Activated);
   end Initialize;
end Wind_Turbine;
