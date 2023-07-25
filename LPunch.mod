MODULE LPunch(NOSTEPIN)
    !********************************************************************
    CONST string Ver_LPunch:="Wooshin 5V01.1 - 2015-05-12 Punching";
    !********************************************************************
    !Library for Punch
    !********************************************************************
    !# -----------------------------------------------
    !# ------ MESSAGE DECLARATIONS
    !# -----------------------------------------------
    LOCAL CONST string RobInLoosen{2}:=["Gripper in loosen position","Gripper är i lossa läge "];
    LOCAL CONST string InfoManual{2}:=["Run in manual mode from here !","Forts?t programköning i manuell mode ! "];
    LOCAL CONST string WaitManual{2}:=["Waiting for manual mode !","Väntar på manuell mode ! "];
    LOCAL CONST string RobInGold{2}:=["Robot in Golden Position","Roboten är i Gyllene läge "];
    LOCAL CONST string RobMovServ{2}:=["The Robot is moving to service position ","Roboten Rör sej mot serviceposition "];
    LOCAL CONST string InServWaitPLC{2}:=["Robot in service position waiting       PLC order to continue  ","Roboten är i service läge, väntar på   PLC order för att fortsätta"];

    !# -----------------------------------------------
    !# ------ TOOL DATA
    !# -----------------------------------------------
    PERS tooldata StatPunch891TCP:=[FALSE, [[-1740.27,-2593.63,1863.56],[0.944827,0.053829,-0.2531,0.200843]], [125,[312.6,-11.4,114],[1,0,0,0],7.622,101.058,108.131]];

    !# -----------------------------------------------
    !# ------ OTHER DECLARATIONS
    !# -----------------------------------------------
    LOCAL CONST Locksignal WaitService:=[102,""];

    !# -----------------------------------------------
    !# ------ GRIPPER EQUIPTMENT
    !# -----------------------------------------------
    !PERS GripperEquip PunchEquip:=["Gripper Equiptment for Punch",2,FALSE,"toxpressesr8510","","","","",4,1,0,TRUE,TRUE,""];

    !# -----------------------------------------------
    !# ------ GRIPPER DATA
    !# -----------------------------------------------
    !(Alarms: [ClampSeqData 2x01-2x40  PartChkdata 2x41-2x60  Vacuumdata 2x61-2x80)
    !: Minimum: (Max 80 signs in each string)
    PERS ClampSeqData PunchClampSeq1Cls:=["Punch - Set Equalizing",
                                         1,                                 
                                         "O_Equalizer_Up",
                                         "O_Equalizer_Down",
                                         FALSE,FALSE,TRUE,
                                         ["I_Equalizer_Up |Missing Signal -Input2 Set Equalizing - 110S1S1",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         ""],
                                         ["I_Equalizer_Down |Signal still on -Input1 ReSet Equalizing - 110S1R1",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         ""],
                                         3,
                                         [2201,FALSE],
                                         [2221,FALSE],
                                         1,TRUE,TRUE,
                                         "Valid - 05:51:16"];

    PERS ClampSeqData PunchClampSeq1Opn:=["Punch - ReSet Equalizing",
                                         1,
                                         "O_Equalizer_Down",
                                         "O_Equalizer_Up",
                                         FALSE,TRUE,FALSE,
                                         ["I_Equalizer_Down |Missing Signal -Input1 ReSet Equalizing - 110S1R1",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         ""],
                                         ["I_Equalizer_Up |Signal still on -Input2 Set Equalizing - 110S1S1",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         ""],
                                         3,
                                         [2202,TRUE],
                                         [2222,FALSE],
                                         1,TRUE,TRUE,
                                         "Valid - 05:51:16"];

    !: Maximum: (Max 80 signs in each string)
    PERS ClampSeqData PunchClampSeq2Cls:=["Punch - Close Punch",
                                         1,
                                         "O_Punch_Down",
                                         "O_Punch_Up",
                                         FALSE,FALSE,TRUE,
                                         ["I_Punch_Down|Missing Signal -Input1 Cls Punch - 110S2S1",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         ""],
                                         ["I_Punch_Up|Signal still on -Input1 Open Punch - 110S2R1",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         ""],
                                         3,
                                         [2203,FALSE],
                                         [2223,FALSE],
                                         1,TRUE,TRUE,
                                         "Valid - 05:51:16"];

    PERS ClampSeqData PunchClampSeq2Opn:=["Punch - Open Punch",
                                         1,
                                         "O_Punch_Up",
                                         "O_Punch_Down",
                                         FALSE,TRUE,FALSE,
                                         ["I_Punch_Up|Missing Signal -Input1 Open Punch - 110S2S1",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         ""],
                                         ["I_Punch_Down|Signal still on -Input1 Cls Punch - 110S2R1",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         "",
                                         ""],
                                         3,
                                         [2204,FALSE],
                                         [2224,FALSE],
                                         1,TRUE,TRUE,
                                         "Valid - 05:51:16"];

    PERS PartChkdata PunchPartChk1:=["Punch - Check Air Present",
                                1,
                                ["I_AirIs_Ok|Check air on Punch tool -Input3 - 110S1E1",
                                "",
                                "",
                                "",
                                "",
                                "",
                                "",
                                "",
                                "",
                                ""],
                                2,
                                [2241,FALSE],
                                [2251,FALSE],
                                TRUE,FALSE,
                                "Valid - 05:51:16"];

    !# -----------------------------------------------
    !# ------ TOOL ID
    !# -----------------------------------------------
    CONST gunnum Punch:=2;

    !# -----------------------------------------------
    !# ------ OTHER DECLARATIONS
    !# -----------------------------------------------

    !PROC ProdSchedule_Example()
    !  ! EXAMPLE for Punch OPEN , CLOSE AND Punching
    !  !  PROD EXAMPLE
    !  !
    !  !Initiate Punch equipment:
    !  IniPunch;
    !  !
    !  !Move towards Punch
    !  MoveL ToPunch1_10,v2500,z10,StatPunch891TCP\Wobj:=D930Car124\TLoad:=D930BSOStn133;
    !  MoveL ToPunch1_20,v2500,z10,StatPunch891TCP\Wobj:=D930Car124\TLoad:=D930BSOStn133;
    !  !
    !  !Move down to Punch Punching tool & Punch:
    !  PunchL Punch1,v150,StatPunch891TCP,D910Car124\TLoad:=Gripp411Load\TLoad:=D930BSOStn133;
    !  !
    !  !Move from Punch
    !  MoveL FrPunch1_10,v2500,z10,StatPunch891TCP\Wobj:=D930Car124\TLoad:=D930BSOStn133;
    !  MoveL FrPunch1_20,v2500,z10,StatPunch891TCP\Wobj:=D930Car124\TLoad:=D930BSOStn133;
    !  !
    !  !End Punch equipment:
    !  EndPunch;
    !  !
    !ENDPROC

    !# -----------------------------------------------
    !# ------ SUB PROCEDURES
    !# -----------------------------------------------


    PROC IniPunch()
      !***************************************
      ! Routine:IniPunch
      ! Description:Init Tool Punch
      !
      !***************************************
      IO_Enable "toxpresses",Unit_timeout,TRUE;
      !InitGripperData Punch,PunchEquip;
      GripperSprVsnON PunchPartChk1;	!Turn on supervision of Air
      PunchOpen;
      GripperClose PunchClampSeq1Cls;	!Set Equalizing
    ENDPROC


    PROC EndPunch()
      !***************************************
      ! Routine:EndPunch
      ! Description:Disable Tool Punch
      !
      !***************************************
      GripperSprVsnOff PunchPartChk1;	!Turn off supervision of Air
      PunchOpen;
      GripperOpen PunchClampSeq1Opn;	!ReSet Equalizing
      PunchClose;
      IO_Disable "grippernode1r8510",Unit_timeout,TRUE;
    ENDPROC


    PROC PunchOpen()
      !***************************************
      ! Routine:PunchOpen
      ! Description:Open Punching Tool
      !
      !***************************************
      GripperOpen PunchClampSeq2Opn;
    ENDPROC


    PROC PunchClose()
      !***************************************
      ! Routine:PunchClose
      ! Description:Close Punching Tool
      !
      !***************************************
      GripperClose PunchClampSeq2Cls;
    ENDPROC


    PROC PunchL(
      robtarget PointName,
      speeddata Speed,
      PERS tooldata Tool,
      PERS wobjdata WObj
      \PERS loaddata TLoad)

      !***************************************
      ! Routine:PunchL
      ! Description:Move down to Tool Punch Bed
      ! then punch the hole &
      ! open Punch tool afterwards
      !***************************************
      PunchOpen;
      MoveL PointName,Speed,fine,Tool\Wobj:=WObj\TLoad?TLoad;
      PunchClose;
      PunchOpen;
    ENDPROC

  ENDMODULE
