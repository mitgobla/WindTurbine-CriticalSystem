pragma SPARK_Mode(On);

with Wind_Turbine; use Wind_Turbine;

procedure Main is

begin
   Initialize;
   loop
      pragma Loop_Invariant(Is_Safe(Status_Turbine));
      Read_Wind_Speed;
      Monitor_Turbine;
      Print_Status;
   end loop;

end Main;
