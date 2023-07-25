MODULE R8401
  !********************************************************************
  CONST string Ver_R8401:="ABB Version 5.1.1  2009-02-16 VCC";
  !********************************************************************
  !******************   ABB IRB F-Pack  *******************************
  !MainRobot module, xxyy refer to robotnumber.
  !********************************************************************
  CONST ee_event PM_E_StartInit:=[EE_START,"Init","T_ROB1",1,1];
  CONST ee_event peEeTestPartFin:=[EE_START,"TestPartFin","",2,255]; 
  CONST ee_event PM_E_StartAutoRun:=[EE_START,"AutoRun","T_ROB1",2,1];
  CONST ee_event PM_E_EquipCheck:=[EE_PRE_PROD,"EquipmentCheck","T_ROB1",1,1]; 
  CONST ee_event PM_E_StartAutoRun2:=[EE_POST_PROD,"AutoRun","T_ROB1",1,1];
  CONST ee_event PM_E_GetOrder:=[EE_WAIT_ORDER,"GetOrder","T_ROB1",1,1];

  PROC main()
    !***************************************
    ! Routine: Main
    ! Description:Mainroutine
    ! ---- READ ONLY --- 
    !***************************************
 
    ExecEngine;
  ENDPROC
ENDMODULE
