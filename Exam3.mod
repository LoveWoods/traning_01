MODULE PanelGrip
    !+++++++++++++++++ Tool data ++++++++++++++++
    PERS tooldata tGripper:=[TRUE,[[-5,-5,338],[1,0,0,0]],[32.5,[31.3,16.5,116.2],[1,0,0,0],1.731,1.275,1.406]];
    !+++++++++++++++++ Load data +++++++++++++++
    PERS loaddata loValve_Body:=[5,[0,0,35],[1,0,0,0],0,0,0];
    !+++++++++++++++++ Speed data +++++++++++++++
    CONST speeddata vE100:=[60,60,5000,1000];
    CONST speeddata vE100_Slow:=[40,40,5000,1000];
    CONST speeddata vE100_Move:=[100,100,5000,1000];
    CONST speeddata vE100_Slow_K1:=[14,14,5000,1000];
    CONST speeddata vE200_Slow:=[100,100,5000,1000];
    CONST speeddata vE200_Move:=[200,200,5000,1000];
    CONST speeddata vF1000_Slow:=[50,50,5000,1000];
    CONST speeddata vF1000_Move:=[80,80,5000,1000];

    !+++++++++++++++ Home Position data +++++++++
    PERS pos posHome:=[354.06,-722.697,1221.08];
    VAR pos posCurrent:=[0,0,0];
    VAR num nChoice:=0;

    !++++++++++++++++ Robot data ++++++++++++++++
    !Robot Home Robtarget Data
    CONST robtarget pHome:=[[353.66,-722.23,1221.43],[0.000228499,0.708591,0.705619,-0.000467409],[-1,-1,0,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];


    PROC rHomeChk()
        home_chk:
        posCurrent:=CPos(\Tool:=tGripper);
        regX:=posCurrent.x-posHome.x;
        regY:=posCurrent.y-posHome.y;
        regZ:=posCurrent.z-posHome.z;
        IF Abs(regX)<30 AND Abs(regY)<30 AND Abs(regZ)<30 THEN
            MoveJ pHome,v300,fine,tGripper;
            Set co04_Robot_Home;
            RETURN ;
        ELSE
            TPErase;
            TPWrite "Can Robot Reach In Safety Position";
            TPReadFK nChoice,"HOME Position?","YES","","","","OK";
            TEST nChoice
            CASE 1:
                MoveJ pHome,v1000,fine,tGripper;
                posHome:=Cpos(\Tool:=tGripper);
            CASE 5:
                TPErase;
                TPWrite "Move Manually The Robot To A Safe Position. Press OK To Continue.";
                TPReadFK nChoice,"","","","","","OK";
            ENDTEST
        ENDIF
        GOTO home_chk;
    ENDPROC

    PROC rZero_Pos()
        !Move to Zero("0")-Position
        TPErase;
        TPWrite "Do you want Robot Zero(0)-Position";
        TPReadFK nChoice,"Zero(0)-Position?","YES","","","","NO";
        TEST nChoice
        CASE 1:
            MoveAbsJ [[0,0,0,0,0,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]]\NoEOffs,v200,fine,tGripper;
            EXIT;
        ENDTEST
    ENDPROC

    PROC rGrip_Close()
        Reset do01_Gr_Open;
        Set do02_Gr_Close;
        WaitDI di02_Gr_Close,1;
        GripLoad loValve_Body;
        WaitTime 0.5;
        RETURN ;
    ENDPROC

    PROC rGrip_Open()
        Reset do02_Gr_Close;
        Set do01_Gr_Open;
        WaitDI di01_Gr_Open,1;
        GripLoad load0;
        WaitTime 0.5;
        RETURN ;
    ENDPROC

    PROC rPickUp_Work()
        WaitUntil ci08_Work_A_Side=1 OR ci09_Work_B_Side=1;
        Reset co04_Robot_Home;
        MoveJ pTurn_Rdy,v3000,z50,tGripper;
        rGrip_Open;
        Set co15_Rob1_Safety;
        Set co08_Turn_Pickup;
        Set co09_ecco200_Run;
        IF ci08_Work_A_Side=1 AND di07_6sok=1 THEN
            rPA1_6sok;
        ELSEIF ci08_Work_A_Side=1 AND di08_Gen2=1 THEN
            rPA2_8sok;
        ELSEIF ci08_Work_A_Side=1 AND ci09_Gen2=1 THEN
            rPA2_G2;
        ELSEIF ci09_Work_B_Side=1 AND di09_Gen2=1 THEN
            rPA2_G2;
        ELSEIF ci09_Work_B_Side=1 AND di08_8sok=1 THEN
            rPB1_8sok;
        ELSEIF ci09_Work_B_Side=1 AND di07_6sok=1 THEN
            rPB2_6sok;
        ENDIF
        Reset co08_Turn_Pickup;
        RETURN ;
    ENDPROC

    PROC rPickUp_B()
        IF nB_Side=0 THEN
            nB_Side=1;
        ENDIF
        Work_Pickup:
        TEST nB_Side
        CASE 1:
            MoveL Offs(pB1_1,0,0,80),v1000,z10,tGripper;
            !MoveL RelTool(pB1_1, 0, 0, 40), v300, z5, tGripper;
            MoveL pB1_1,v300,z1,tGripper;
            MoveL pB1_11,v50,fine,tGripper;
            rGrip_Close;
            IF di03_Work_Chk=0 THEN
                nB_Side:=2;
                WaitTime 1;
                MoveL Offs(pB1_1,0,0,80),v300,z10,tGripper;
                rGrip_Open;
                GOTO Work_Pickup;
            ENDIF
            MoveL Offs(pB1_1,0,0,30),v300,z1,tGripper;
            MoveL Offs(pb1_1,0,0,80),v1000,z10,tGripper;
            nB_Side:=2;
        CASE 2:
            MoveL Offs(pB1_2,0,0,130),v2000,z20,tGripper;
            MoveL Offs(pB1_2,0,0,80),v1000,z10,tGripper;
            MoveL Offs(pB1_2,0,0,40),v300,z5,tGripper;
            MoveL pB1_2,v50,fine,tGripper;
            rGrip_Close;
            IF di03_Work_Chk=0 THEN
                nB_Side:=3;
                WaitTime 1;
                MoveL Offs(pB1_2,0,0,130),v300,z10,tGripper;
                rGrip_Open;
                GOTO Work_Pickup;
            ENDIF
            MoveL Offs(pB1_2,0,0,30),v300,z1,tGripper;
            MoveL Offs(pB1_2,0,0,130),v1000,z10,tGripper;
            nB_Side:=3;
        CASE 3:
            MoveL Offs(pB1_2,0,0,180),v2000,z20,tGripper;
            MoveL Offs(pB1_3,0,0,80),v1000,z10,tGripper;
            MoveL Offs(pB1_3,0,0,40),v300,z5,tGripper;
            MoveL pB1_3,v50,fine,tGripper;
            rGrip_Close;
            IF di03_Work_Chi=0 THEN
                nB_Side:=4;
                WaitTime 1;
                MoveL Offs(pB1_3,0,0,180),v300,z10,tGripper;
                rGrip_Open;
                GOTO Work_Picikup;
            ENDIF
            MoveL Offs(pB1_3,0,0,30),v300,z1,tGripper;
            MoveL Offs(pB1_3,0,0,180),v1000,z10,tGripper;
            nB_Side:=4;
        ENDTEST
        MoveJ pTurn_Rdy,v7000,z50,tGripper;
        RETURN ;

    ENDPROC

ENDMODULE