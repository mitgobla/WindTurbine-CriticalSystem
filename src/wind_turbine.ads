pragma SPARK_Mode (On);

with SPARK.Text_IO; use SPARK.Text_IO;

package Wind_Turbine is

   -- This is the fastest wind speed (mph) ever recorded during the passage of Tropical Cyclone Olivia on 10 April 1996
   -- It is used as a maximum value for sensor input, as we can assume we will never see values higher than this.
   -- https://en.wikipedia.org/wiki/Wind_speed#Highest_speed
   Maximum_Wind_Speed : constant Integer := 253;

   -- This is the typical survival speed (mph) of commercial wind turbine
   -- If the wind turbine continues to rotate above this speed, it will start to become damaged
   -- https://en.wikipedia.org/wiki/Wind_turbine_design#Power_control
   Critical_Wind_Speed : constant Integer := 134;

   -- The typical turbine will begin to slow down its rotation when it reaches over this speed (mph)
   -- https://www.kvcc.edu/about/sustainability/images/Wind_FAQ.pdf
   Shutdown_Wind_Speed : constant Integer := 55;

   -- The typical turbine will only allow itself to start rotating if the wind is over this speed (mph)
   -- This is due to it needing enough kinetic energy to complete a rotation and produce useful power
   -- https://www.energy.gov/eere/wind/how-wind-turbine-works-text-version
   Startup_Wind_Speed : constant Integer := 7;

   type Speed_Range is new Integer range 0 .. Maximum_Wind_Speed;

   -- On the tips of wind turbines, they can rotate up to 90 degrees to stall the blades after a few rotations.
   -- The aerodynamic braking system is the usual way of slowing or stopping a wind turbine
   type Status_Tip_Brake_Type is (Activated, Deactivated);

   -- The mechanical brake is only used as a backup system for the aerodynamic braking system.
   -- It is also used to park the turbine during maintenance or bouts of extremely severe weather.
   type Status_Mechanical_Brake_Type is (Activated, Deactivated);

   -- Overall status of the wind turbine as a record, consisting of measured speed, tip brake status and mechanical brake status
   type Status_Turbine_Type is record
      Speed_Measured          : Speed_Range;
      Status_Tip_Brake        : Status_Tip_Brake_Type;
      Status_Mechanical_Brake : Status_Mechanical_Brake_Type;
   end record;

   -- Global status of the turbine
   Status_Turbine : Status_Turbine_Type;

   procedure Read_Wind_Speed with
      Global  => (In_Out => (Standard_Output, Standard_Input, Status_Turbine)),
      Depends => (Standard_Output => (Standard_Output, Standard_Input),
       Standard_Input => Standard_Input,
       Status_Turbine => (Status_Turbine, Standard_Input));

   function Status_Tip_Brake_To_String
     (Status_Tip_Brake : Status_Tip_Brake_Type) return String;

   function Status_Mechanical_Brake_To_String
     (Status_Mechanical_Brake : Status_Mechanical_Brake_Type) return String;

   procedure Print_Status with
      Global  => (In_Out => Standard_Output, Input => Status_Turbine),
      Depends => (Standard_Output => (Standard_Output, Status_Turbine));

   function Is_Safe (Status : Status_Turbine_Type) return Boolean is
     (if
        (Integer (Status.Speed_Measured) > Critical_Wind_Speed) or
        (Integer (Status.Speed_Measured) <= Startup_Wind_Speed)
      then
        Status.Status_Tip_Brake = Activated and
        Status.Status_Mechanical_Brake = Activated
      elsif (Integer (Status.Speed_Measured) > Shutdown_Wind_Speed) then
        Status.Status_Tip_Brake = Activated and
        Status.Status_Mechanical_Brake = Deactivated
      else Status.Status_Tip_Brake = Deactivated and
        Status.Status_Mechanical_Brake = Deactivated);

   procedure Monitor_Turbine with
      Global  => (In_Out => Status_Turbine),
      Depends => (Status_Turbine => Status_Turbine),
      Post    => Is_Safe (Status_Turbine);

   procedure Initialize with
      Global  => (Output => (Standard_Output, Standard_Input, Status_Turbine)),
      Depends => ((Standard_Output, Standard_Input, Status_Turbine) => null),
      Post    => Is_Safe (Status_Turbine);

end Wind_Turbine;
