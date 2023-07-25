MODULE BEAM(SYSMODULE,NOSTEPIN)
  !***************************************************
  !Module Name: TCP Measurment
  !Version:     1
  !Programtype: Program module Foreground
  !Description: TCP measurment support
  !Date:        040502
  !Author:
  !Revision:    0
  !*****************************************************
  PERS string versionBEAM:="ABB 5.0.1 - 2012-11-09 ";
  !*****************************************************
  CONST string E_TXT_BCHNOM1{2}:=[" differ from nominal TCP"," skiljer från nominell TCP"];
  CONST string E_TXT_BERRIN{2}:=["In Tool ","I verktyg "];
  CONST string T_TXT_MEAS1{2}:=["Measuring  1:","Mätning    1:"];
  CONST string T_TXT_MEAS2{2}:=["Measuring  2:","Mätning    2:"];
  CONST string T_TXT_ALIGN{2}:=["Align the gun or change it","Rikta upp eller byt tång"];
  CONST string T_TXT_RUNCALIB{2}:=["Run the TCP-calibration again.","Kör TCP-kalibreringen igen"];
  CONST string T_TXT_BREAKNOCHA{2}:=["BREAK, no change of TCP value is done.","BREAK, ingen uppdatering av TCP värde   görs."];
  CONST string T_TXT_DISPNOZZAN{2}:=["DispNozzel with angel?","Vinklat limmunstycke?"];
  CONST string T_TXT_NOZZROT{2}:=["Type in the rotation angel of the nozzel","Välj limmunstyckets rotationsvinkel"];
  CONST string T_TXT_NO24DEG{2}:=["Reorientation to small for QuickCheck","Omorientering liten för QuickCheck"];

  LOCAL PERS pose poseReady:=[[0,0,0],[1,0,0,0]];
  LOCAL PERS pose poseReadyOffset:=[[736.422,-0.799269,445.287],[1,0,0,0]];
  LOCAL PERS pose nyPoseBeam:=[[736.422,-0.799269,445.287],[1,0,0,0]];
  LOCAL PERS pose inPoseBeam:=[[736.55,-0.68,445.21],[1,0,0,0]];
  PERS tooldata tBeam:=[TRUE,[[736.422,-0.799269,445.287],[0.499922,-0.500047,-0.500033,0.499998]],[147.4,[46.8,24.3,349.4],[1,0,0,0],10.342,16.393,7.632]];
  PERS tooldata tBeamRobCad:=[TRUE,[[736.422,-0.799269,445.287],[0.143103,-0.692917,0.130704,-0.694481]],[0,[0,0,0],[1,0,0,0],0,0,0]];
  PERS tooldata Beamtool:=[TRUE,[[736.422,-0.799269,445.287],[1,0,0,0]],[147.4,[46.8,24.3,349.4],[1,0,0,0],10.342,16.393,7.632]];
  LOCAL PERS tooldata tmptoolBeam:=[TRUE,[[736.55,-0.68,445.21],[1,0,0,0]],[147.4,[46.8,24.3,349.4],[1,0,0,0],10.342,16.393,7.632]];
  LOCAL PERS tooldata tool:=[TRUE,[[284.49,-0.15,889.01],[0.707107,0,-0.707107,0]],[142.8,[24.2,-4.5,306.5],[1,0,0,0],3.729,9.971,5.731]];
  LOCAL VAR robtarget pnt:=[[0,0,0],[1,0,0,0],[0,0,-1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]];
  LOCAL PERS wobjdata tmpwobjBeam:=[FALSE,TRUE,"",[[5542.22,67.2464,599.291],[0.701608,0.00932645,-0.712459,-0.00787355]],[[0,0,0],[0.707107,0,0,-0.707107]]];
  LOCAL PERS wobjdata wCurrWobj:=[FALSE,TRUE,"",[[5541.55,67.1226,585.996],[0.000345799,-1,-3.70465E-05,0.000364107]],[[0,0,0],[1,0,0,0]]];
  LOCAL VAR iodev file;
  LOCAL PERS errnum errBeamSensor:=71;
  LOCAL PERS errnum errRetry:=72;
  LOCAL PERS errnum noErrPosTest:=73;
  PERS num t1xTol:=1.5;
  PERS num t2xTol:=1.5;
  PERS num t3xTol:=1.5;
  PERS num t4xTol:=1.5;
  PERS num t5xTol:=1.5;

  PERS num t1yTol:=1.5;
  PERS num t2yTol:=1.5;
  PERS num t3yTol:=1.5;
  PERS num t4yTol:=1.5;
  PERS num t5yTol:=1.5;

  PERS num t1zTol:=1.5;
  PERS num t2zTol:=1.5;
  PERS num t3zTol:=1.5;
  PERS num t4zTol:=1.5;
  PERS num t5zTol:=1.5;

  PERS num t1xQTol:=0.4;
  PERS num t2xQTol:=0.4;
  PERS num t3xQTol:=0.4;
  PERS num t4xQTol:=0.4;
  PERS num t5xQTol:=0.4;

  PERS num t1yQTol:=0.4;
  PERS num t2yQTol:=0.4;
  PERS num t3yQTol:=0.4;
  PERS num t4yQTol:=0.4;
  PERS num t5yQTol:=0.4;

  PERS num t1zQTol:=1.5;
  PERS num t2zQTol:=1.5;
  PERS num t3zQTol:=1.5;
  PERS num t4zQTol:=1.5;
  PERS num t5zQTol:=1.5;

  PERS num t1zWeartool:=-10;
  PERS num t2zWeartool:=-10;
  PERS num t3zWeartool:=-10;
  PERS num t4zWeartool:=-10;
  PERS num t5zWeartool:=-10;

  PERS num t1Day1Tol:=10;
  PERS num t2Day1Tol:=10;
  PERS num t3Day1Tol:=10;
  PERS num t4Day1Tol:=10;
  PERS num t5Day1Tol:=10;

  PERS num chTcpPosTol:=30;
  PERS num maxLogCounter:=200;
  PERS num logCounterBeam:=13;
  PERS num tXxTol{5}:=[1.5,1.5,1.5,1.5,1.5];
  PERS num tXyTol{5}:=[1.5,1.5,1.5,1.5,1.5];
  PERS num tXzTol{5}:=[1.5,1.5,1.5,1.5,1.5];
  
  PERS num tXxQTol{5}:=[0.4,0.4,0.4,0.4,0.4];
  PERS num tXyQTol{5}:=[0.4,0.4,0.4,0.4,0.4];
  PERS num tXzQTol{5}:=[1.5,1.5,1.5,1.5,1.5];
  
  
  
  LOCAL PERS num tXzWTool{5}:=[10,10,10,10,10];
  PERS num tXD1Tol{5}:=[10,10,10,10,10];
  LOCAL PERS num diffXBeam:=-0.1;
  LOCAL PERS num diffYBeam:=-0.1;
  LOCAL PERS num diffZBeam:=0.1;
  LOCAL PERS num diffXD1:=2.3;
  LOCAL PERS num diffYD1:=-0.1;
  LOCAL PERS num diffZD1:=0.2;
  PERS num diffX1:=1.8;
  PERS num diffY1:=-0.1;
  PERS num diffZ1:=0;
  PERS num diffX2:=0.5;
  PERS num diffY2:=-0.2;
  PERS num diffZ2:=2.6;
  PERS num diffX:=-0.6;
  PERS num diffY:=0.2;
  PERS num diffZ:=-0.0999999;
  PERS num chTcpMaxDiff:=1;
  PERS num BEAMsettleTime:=0.5;
  LOCAL PERS string tNameBeam:="Gun311RefTCP";
  LOCAL PERS string file1Beam:="TCP.log";
  LOCAL PERS string file2Beam:="TCP_old.log";
  LOCAL VAR string txtXFirstCh:="";
  LOCAL VAR string txtYFirstCh:="";
  LOCAL VAR string txtZFirstCh:="";
  LOCAL VAR bool firstCheck:=TRUE;
  LOCAL VAR bool define:=FALSE;
  LOCAL VAR bool measError:=FALSE;
  LOCAL PERS bool zTol:=FALSE;
  VAR bool disableLogWrite:=FALSE;
  LOCAL VAR bool log_error:=TRUE;
  LOCAL CONST errnum err_pos:=2;
  LOCAL CONST errnum err_rot:=5;
  
!  LOCAL CONST pose poseDispAngel{5}:=[[[-6.925,0,-1.095],[0.906308,0,-0.422618,0]],[[0,6.925,-1.095],[0.640857,0.298836,-0.298836,0.640856]],[[6.925,0,-1.095],[0,0.422618,0,0.906308]],[[0,-6.925,-1.095],[0.640857,-0.298836,-0.298836,-0.640856]],[[0,0,0],[1,0,0,0]]];
!  LOCAL CONST pose poseDispAngel{5}:=[[[6.925,0,-1.095],[0,0.422618,0,0.906308]],[[0,-6.925,-1.095],[0.640857,-0.298836,-0.298836,-0.640856]],[[-6.925,0,-1.095],[0.906308,0,-0.422618,0]],[[0,6.925,-1.095],[0.640857,0.298836,-0.298836,0.640856]],[[0,0,0],[1,0,0,0]]];


  LOCAL CONST pose poseDispAngel{5}:=[[[6.925,0,-1.095],[0.906308,0,0.422618,0]],[[0,-6.925,-1.095],[0.640856,0.640856,0.298836,-0.298836]],[[-6.925,0,-1.095],[0,-0.422618,0,0.906308]],[[0,6.925,-1.095],[0.640856,-0.298836,0.298836,0.640856]],[[0,0,0],[1,0,0,0]]];


  PERS robtarget pPlusYRotNoTiltL{5}:=[[[0.402922,-12.2324,-2.17903],[0.990304,-0.00976114,0.138173,0.0105827],[-1,0,1,0],[2999.92,20.0062,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[0,-3,3,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[0,-3,3,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[-1,-2,1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[0,-3,3,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]]];
  PERS robtarget pMinusYRotNoTitL{5}:=[[[0.438765,-12.2754,-2.01714],[0.990025,-0.00648799,-0.140154,0.0128719],[0,0,2,0],[2999.92,20.008,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[0,-3,3,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[0,-3,3,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[-1,-2,1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[0,-3,3,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]]];
  PERS robtarget pFaceNoTiltLaRun{5}:=[[[0.369139,2.11575,-1.56854],[0.999895,-0.00822721,-0.000963116,0.0118589],[0,0,2,0],[2999.94,20.0067,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[0,-3,3,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[0,-3,3,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[-1,-2,1,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]],[[0,0,0],[1,0,0,0],[0,-3,3,0],[9E+09,9E+09,9E+09,9E+09,9E+09,9E+09]]];
  
  !**************************************************************
  ! Rutine name :SetupBeam
  ! Description :Used in manual to define a new TCP
  !**************************************************************
  PROC SetupBeam(
    INOUT tooldata toolIn, tooldata nomtool,
    gunnum setupNo)

    VAR num key_in:=0;
    VAR jointtarget tmpJoinT;
    VAR pos oldpsCyltoFace;
    VAR pos newpsCyltoFace;
    VAR orient oriToolY;
    VAR pose pose1;
    VAR pose pose2;
    VAR pose pose3;
    VAR pose pose4;
    VAR bool updateToolhome:=FALSE;
    VAR num transX;
    VAR num transY;
    VAR num transZ;
    VAR bool TolError;
    VAR bool TolWarning;
    VAR string txtX;
    VAR string txtY;
    VAR string txtZ;
    
    
    tool:=toolIn;
    !TPReadFK key_in,T_TXT_DISPNOZZAN{Language},"","","",T_TXT_NO{Language},T_TXT_YES{Language};
    rDisplayFK key_in,strDISPNOZZANGEL,stBuNoYes_4_5;
    TEST key_in
    CASE 4:
      bDispOffset{setupNo}:=FALSE;
    CASE 5:
      bDispOffset{setupNo}:=TRUE;   
      tool.tframe.rot:=poseDispAngel{5}.rot;
    ENDTEST
    tDay1{setupNo}:=nomtool;
    !    IF toolhome=tool updateToolhome:=TRUE;
    define:=TRUE;
    nGun:=setupNo;
    SetSysData toolIn;
    SetSysData wobj0;
    DefTol zTol;
    GetSysData toolIn\ObjectName:=tNameBeam;
    !TPErase;
    !TPWrite T_TXT_BTOOL{Language}+tNameBeam;
    !TPWrite T_TXT_BSETUP{Language}\Num:=setupNo;
    !TPReadFK key_in,"","","","",T_TXT_SKIP,T_TXT_OK;
    strBsetupBool{7}:="   "+T_TXT_BTOOL{Language}+tNameBeam;    
    strBsetupBool{18}:="   "+T_TXT_BTOOL{Language}+tNameBeam;
    strBsetupBool{10}:="   "+T_TXT_BSETUP{Language}+(NumToStr(setupNo,0));   
    strBsetupBool{21}:="   "+T_TXT_BSETUP{Language}+(NumToStr(setupNo,0));
    rDisplayFK key_in,strBsetupBool,stBuSkipOk_4_5;
    IF key_in=4 RETURN;
    IF NOT bDispOffset{setupNo} THEN
      !TPWrite T_TXT_BORMUDEF{Language};
      !TPReadFK key_in,T_TXT_BORIENTQ{Language},"","","",T_TXT_YES{Language},T_TXT_NO{Language};
      rDisplayFK key_in,strOrientMustDefBefore,stBuYesNo;
      IF key_in=4 THEN
        !TPReadFK key_in,T_TXT_BSYNKPOSQ{Language},"","","",T_TXT_YES{Language},T_TXT_NO{Language};
        rDisplayFK key_in,strStartFromSync,stBuYesNo;
        IF key_in=4 THEN
          !TPWrite T_TXT_BMOSYMAN{Language};
          !TPReadFK key_in,T_TXT_BBOK{Language},"","","","",T_TXT_OK;
          rDisplayFK key_in,strJogManCloseToSync,stBuOk_5;
          Stop\NoRegain;
          maincal;
        ENDIF
        !TPWrite T_TXT_BMOCO1{Language};
        !TPWrite T_TXT_BMOCO2{Language};
        !TPWrite T_TXT_BMOCO3{Language};
        !TPWrite T_TXT_BMOCO4{Language};
        !TPReadFK key_in,T_TXT_BBOK{Language},"","","","",T_TXT_OK;
        rDisplayFK key_in,strJogManToToolOrient,stBuOk_5;        
        !      Motionsup\OFF;
        Stop\NoRegain;
        !      MotionSup\On\TuneValue:=MotSupTuneStd;
        pnt:=CRobT(\Tool:=tool0\WObj:=wobj0);
        dtori tool,wobj0;
        TPErase;
        !TPWrite tNameBeam+".tframe.rot.q1= "\Num:=tool.tframe.rot.q1;
        !TPWrite tNameBeam+".tframe.rot.q2= "\Num:=tool.tframe.rot.q2;
        !TPWrite tNameBeam+".tframe.rot.q3= "\Num:=tool.tframe.rot.q3;
        !TPWrite tNameBeam+".tframe.rot.q4= "\Num:=tool.tframe.rot.q4;
        !TPWrite tNameBeam+T_TXT_UPDORI1{Language}+T_TXT_UPDORI2{Language};
        !TPReadFK key_in,"","","","","",T_TXT_UPDATE{Language};
        
        strToolOrientResult{6}:="   "+tNameBeam+".tframe.rot.q1= "+(NumToStr(tool.tframe.rot.q1,0));    
        strToolOrientResult{17}:="   "+tNameBeam+".tframe.rot.q1= "+(NumToStr(tool.tframe.rot.q1,0));        
        strToolOrientResult{7}:="   "+tNameBeam+".tframe.rot.q2= "+(NumToStr(tool.tframe.rot.q2,0));    
        strToolOrientResult{18}:="   "+tNameBeam+".tframe.rot.q2= "+(NumToStr(tool.tframe.rot.q2,0));
        strToolOrientResult{8}:="   "+tNameBeam+".tframe.rot.q3= "+(NumToStr(tool.tframe.rot.q3,0));    
        strToolOrientResult{19}:="   "+tNameBeam+".tframe.rot.q3= "+(NumToStr(tool.tframe.rot.q3,0));        
        strToolOrientResult{9}:="   "+tNameBeam+".tframe.rot.q4= "+(NumToStr(tool.tframe.rot.q4,0));    
        strToolOrientResult{20}:="   "+tNameBeam+".tframe.rot.q4= "+(NumToStr(tool.tframe.rot.q4,0));
        strToolOrientResult{10}:="   "+tNameBeam+T_TXT_UPDORI1{Language}+T_TXT_UPDORI2{Language};   
        strToolOrientResult{21}:="   "+tNameBeam+T_TXT_UPDORI1{Language}+T_TXT_UPDORI2{Language};
        rDisplayFK key_in,strToolOrientResult,stButtonUpdate_5;
        
        tDay1User{setupNo}:=nomtool;
        tDay1User{setupNo}.tframe.rot:=tool.tframe.rot;
        tBeamLastRunUser{setupNo}.tframe.rot:=tool.tframe.rot;
        toolIn.tframe.rot:=tool.tframe.rot;
        SetSysData toolIn;
        SetSysData wobj0;
      ENDIF
      !TPErase;
      !TPReadFK key_in,T_TXT_BORIPARRAL{Language},"","","",T_TXT_NO{Language},T_TXT_YES{Language};
      rDisplayFK key_in,strBORIPARRAL,stBuNoYes_4_5;
      IF key_in=4 THEN
        TPErase;
        !TPReadFK key_in,T_TXT_BENTERVAL{Language},"","",T_TXT_KEY,"15",T_TXT_SKIP;
        rDisplayFK key_in,strBENTERVAL,stBuKey15Skip_3_4_5;
        IF key_in=3 OR key_in=4 THEN
          IF key_in=3 THEN
            TPReadNum key_in,T_TXT_BENTERVAL{Language}; 
            oriToolY:=OrientZYX(0,key_in,0);
          ENDIF
          IF key_in=4 THEN
            oriToolY:=OrientZYX(0,15,0);
          ENDIF
          pose1:=tool.tframe;
          pose2.rot:=oriToolY;
          pose3:=PoseMult(pose1,pose2);
          tool.tframe.rot:=pose3.rot;
        ENDIF
      ENDIF
      TPErase;
    ENDIF
    !TPReadFK key_in,T_TXT_GOSLOW{Language},"","","",T_TXT_YES{Language},T_TXT_NO{Language};
    rDisplayFK key_in,strGOSLOW,stBuYesNo;
    vFastSearch:=[100,10,0,0];
    vMediumSearch:=[50,10,0,0];
    vSlowSearch:=[20,10,0,0];
    vEstimating:=[10,2,0,0];
    vGunSettle:=[80,30,0,0];
    IF key_in=4 THEN
      vFastSearch:=[40,10,0,0];
      vMediumSearch:=[20,10,0,0];
      vSlowSearch:=[10,10,0,0];
      vGunSettle:=[25,30,0,0];
      BEAMsettleTime:=0.5;
    ENDIF
    TPErase;
    IF DInput(sen1)=1 THEN
      !TPWrite T_TXT_BMOV1{Language};
      !TPWrite T_TXT_BMOV2{Language};
      !TPWrite T_TXT_BMOV3{Language};
      !TPReadFK key_in,T_TXT_BBOK{Language},"","","","",T_TXT_OK;
      rDisplayFK key_in,strJoggStartPos,stBuOk_5;
      Stop\NoRegain;
    ENDIF
    oldpsCyltoFace:=psCyltoFace{setupNo};
    BEAM_Setup tool;
    IF bDispOffset{setupNo} THEN
      !TPReadFK key_in,T_TXT_NOZZROT{Language},"0","90","180","270",T_TXT_SKIP;
      rDisplayFK key_in,strNOZZROT,stBu_0_90_180_270;
      TEST key_in
      CASE 1,2,3,4:    
        tBeam.tframe.rot:=poseDispAngel{key_in}.rot;
        tBeamLastRunUser{setupNo}.tframe.rot:=tBeam.tframe.rot;
        tool.tframe.rot:=tBeam.tframe.rot;
        tDay1User{setupNo}.tframe.rot:=tBeam.tframe.rot;
        psOffsetDisp{setupNo}.x:=poseDispAngel{key_in}.trans.x;
        psOffsetDisp{setupNo}.y:=poseDispAngel{key_in}.trans.y;
        psOffsetDisp{setupNo}.z:=poseDispAngel{key_in}.trans.z;
        CalcDispOffSet tBeam,setupNo;
      CASE 5:
        bDispOffset{setupNo}:=FALSE;
      ENDTEST
    ELSE
      tDay1User{setupNo}:=nomtool;
      tDay1User{setupNo}.tframe.rot:=tool.tframe.rot;	
    ENDIF         
 
    !Ask for offset the TCP in x and z
!    TPWrite T_TXT_BTCPOK1{Language};
!    TPWrite T_TXT_BTCPOK2{Language};
!    TPWrite T_TXT_BTCPOK3{Language};
!    TPWrite T_TXT_BTCPOK4{Language};
!    TPWrite T_TXT_BTCPOK5{Language};
!    TPWrite T_TXT_BTCPOK6{Language};
!    TPWrite T_TXT_BEFOFFX{Language}\Num:=poseOffset{setupNo}.trans.x;
!    TPWrite T_TXT_BEFOFFZ{Language}\Num:=poseOffset{setupNo}.trans.z;
!    TPReadFK key_in,T_TXT_BOFFSOK{Language},"","","",T_TXT_YES{Language},T_TXT_NO{Language};

    strOffsetTCP{9}:="   "+T_TXT_BEFOFFX{Language}+(NumToStr(poseOffset{setupNo}.trans.x,0));    
    strOffsetTCP{20}:="   "+T_TXT_BEFOFFX{Language}+(NumToStr(poseOffset{setupNo}.trans.x,0));
    strOffsetTCP{10}:="   "+T_TXT_BEFOFFZ{Language}+(NumToStr(poseOffset{setupNo}.trans.z,0));   
    strOffsetTCP{21}:="   "+T_TXT_BEFOFFZ{Language}+(NumToStr(poseOffset{setupNo}.trans.z,0));
    rDisplayFK key_in,strOffsetTCP,stBuYesNo;    
    IF key_in=5 poseOffset{setupNo}.trans:=[0,0,0];
    IF key_in=4 THEN
      pose4.trans:=poseOffset{setupNo}.trans;
      tBeam.tframe.rot:=tDay1User{setupNo}.tframe.rot;
      CalcOffSet tBeam,setupNo;
    ENDIF
    !Ask for dist. face to cylinder for checkTCP
    newpsCyltoFace:=psCyltoFace{setupNo};
!    TPErase;
!    TPWrite T_TXT_DIFACY{Language};
!    TPWrite T_TXT_OLDDI{Language}\Num:=Trunc(oldpsCyltoFace.y\Dec:=2);
!    TPWrite T_TXT_NEWDI{Language}\Num:=Trunc(newpsCyltoFace.y\Dec:=2);
!    TPWrite T_TXT_MANDI{Language};
!    TPReadFK key_in,T_TXT_WHDI{Language},T_TXT_OLD{Language},T_TXT_NEW{Language},T_TXT_MAN{Language},"","";

    strDistanceFaceCyl{7}:="   "+T_TXT_OLDDI{Language}+(NumToStr(oldpsCyltoFace.y,2));    
    strDistanceFaceCyl{18}:="   "+T_TXT_OLDDI{Language}+(NumToStr(oldpsCyltoFace.y,2));
    strDistanceFaceCyl{8}:="   "+T_TXT_NEWDI{Language}+(NumToStr(newpsCyltoFace.y,2));   
    strDistanceFaceCyl{19}:="   "+T_TXT_NEWDI{Language}+(NumToStr(newpsCyltoFace.y,2));
    rDisplayFK key_in,strDistanceFaceCyl,stBuOldNewMan_1_2_3;

    IF key_in=1 THEN
      newpsCyltoFace.y:=Trunc(oldpsCyltoFace.y\Dec:=2);
    ENDIF
    IF key_in=2 THEN
      newpsCyltoFace.y:=Trunc(newpsCyltoFace.y\Dec:=2);
    ENDIF
    IF key_in=3 THEN
      TPReadNum key_in,T_TXT_SPECDI{Language};
      newpsCyltoFace.y:=key_in;
    ENDIF
    Beamtool:=tBeam;
    !Ask for updating Setup or not.
    TPErase;
!    TPWrite "";
!    TPWrite T_TXT_SETUPDATE{Language}\Num:=setupNo;
!    TPWrite T_TXT_MEASTOOL{Language};
!    TPWrite "trans.x= "\Num:=Trunc(tBeam.tframe.trans.x\Dec:=2);
!    TPWrite "trans.y= "\Num:=Trunc(tBeam.tframe.trans.y\Dec:=2);
!    TPWrite "trans.z= "\Num:=Trunc(tBeam.tframe.trans.z\Dec:=2);
    transX:=Trunc(tBeam.tframe.trans.x\Dec:=2);
    transY:=Trunc(tBeam.tframe.trans.y\Dec:=2);
    transZ:=Trunc(tBeam.tframe.trans.z\Dec:=2);
    vKoord tDay1User{setupNo},diffXD1,diffYD1,diffZD1;
    TolWarning:=FALSE;
    IF diffXD1>tXD1Tol{setupNo} OR diffXD1<-tXD1Tol{setupNo} TolWarning:=TRUE;
    IF diffYD1>tXD1Tol{setupNo} OR diffYD1<-tXD1Tol{setupNo} TolWarning:=TRUE;
    IF diffZD1>tXD1Tol{setupNo} OR diffZD1<-tXD1Tol{setupNo} TolWarning:=TRUE;
    txtX:=NumToStr(diffXD1,1);
    txtY:=NumToStr(diffYD1,1);
    txtZ:=NumToStr(diffZD1,1);

    strMeasResult{3}:="   "+T_TXT_SETUPDATE{Language}+(NumToStr(setupNo,0));    
    strMeasResult{14}:="   "+T_TXT_SETUPDATE{Language}+(NumToStr(setupNo,0));
    strMeasResult{6}:="   "+"trans.x= "+(NumToStr(tBeam.tframe.trans.x,2));    
    strMeasResult{17}:="   "+"trans.x= "+(NumToStr(tBeam.tframe.trans.x,2));
    strMeasResult{7}:="   "+"trans.y= "+(NumToStr(tBeam.tframe.trans.y,2));    
    strMeasResult{18}:="   "+"trans.y= "+(NumToStr(tBeam.tframe.trans.y,2));
    strMeasResult{8}:="   "+"trans.z= "+(NumToStr(tBeam.tframe.trans.z,2));    
    strMeasResult{19}:="   "+"trans.z= "+(NumToStr(tBeam.tframe.trans.z,2));
    strMeasResult{9}:="   "+tNameBeam+E_TXT_BCHNOM1{Language};    
    strMeasResult{20}:="   "+tNameBeam+E_TXT_BCHNOM1{Language};
    strMeasResult{10}:="   "+E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtXFirstCh,6)+" Y:"+RightShiftString(txtYFirstCh,6)+" Z:"+RightShiftString(txtZFirstCh,6)+" mm";
    strMeasResult{21}:="   "+E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtXFirstCh,6)+" Y:"+RightShiftString(txtYFirstCh,6)+" Z:"+RightShiftString(txtZFirstCh,6)+" mm";
    IF TolWarning THEN
      TPErase;
!      TPWrite T_TXT_BWARNING{Language};
!      TPWrite tNameBeam+E_TXT_BCHNOM1{Language};
!      TPWrite E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtX,6)+" Y:"+RightShiftString(txtY,6)+" Z:"+RightShiftString(txtZ,6)+" mm";      
!      TPWrite T_TXT_ALIGN{Language};
      ! Run the TCP-calibration again.
!      TPWrite T_TXT_RUNCALIB{Language};
      ! BREAK, no change of TCP value is done
!      TPReadFK key_in,T_TXT_BREAKNOCHA{Language},"","","BREAK","","";
      strMeasResult{11}:="   "+T_TXT_BREAKNOCHA{Language};
      strMeasResult{22}:="   "+T_TXT_BREAKNOCHA{Language};
      rDisplayFK key_in,strMeasResult,stBuBreak_3;	
!     TPReadFK key_in,T_TXT_UPDTCP{Language}+tNameBeam+" ?","",T_TXT_NO,"","","";
    ELSE 
      TPErase;
!      TPWrite T_TXT_BWARNING{Language};
!      TPWrite tNameBeam+E_TXT_BCHNOM1{Language};
!      TPWrite E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtX,6)+" Y:"+RightShiftString(txtY,6)+" Z:"+RightShiftString(txtZ,6)+" mm";          	 
!      TPReadFK key_in,T_TXT_UPDTCP{Language}+tNameBeam+" ?",T_TXT_UPDATE{Language},T_TXT_NO{Language},"","","";
      strMeasResult{11}:="   "+T_TXT_UPDTCP{Language};
      strMeasResult{22}:="   "+T_TXT_UPDTCP{Language};
      rDisplayFK key_in,strMeasResult,stButtonUpdateNo_1_2;
    ENDIF
    IF key_in=1 THEN
      tool.tframe.trans.x:=Trunc(tBeam.tframe.trans.x\Dec:=2);
      tool.tframe.trans.y:=Trunc(tBeam.tframe.trans.y\Dec:=2);
      tool.tframe.trans.z:=Trunc(tBeam.tframe.trans.z\Dec:=2);
      tDay1User{setupNo}:=nomtool;
      tBeamLastRunUser{setupNo}:=tool;
      toolIn.tframe.trans:=tool.tframe.trans;
      IF bDispOffset{setupNo} toolIn.tframe.rot:=tool.tframe.rot;
      psCyltoFace{setupNo}:=newpsCyltoFace;
      pPlusYRotNoTilt:=pPlusYRotNoTiltL;
      pMinusYRotNoTilt:=pMinusYRotNoTitL;
      pFaceNoTilt:=pFaceNoTiltLaRun;
      define:=FALSE;
    ENDIF
    !    IF updateToolhome toolhome:=tool;
    SetSysData toolIn;
    SetSysData wobj0;
    IF NOT TolWarning THEN
      IF key_in=2 poseOffset{setupNo}.trans:=pose4.trans;
      !TPReadFK key_in,T_TXT_CHTCP{Language},"","","",T_TXT_YES{Language},T_TXT_NO{Language};
      rDisplayFK key_in,strRunCheckTCP,stBuYesNo;     
      IF key_in=4 CheckTCP toolIn,nomtool,setupNo;
      Stop;
    ENDIF  
    IF TolWarning STOP;  
  ENDPROC

  !**************************************************************
  LOCAL PROC dtori(
    PERS tooldata ttool,
    PERS wobjdata twobj)

    VAR pose pse2:=[[0,0,0],[1,0,0,0]];
    VAR pose pse3:=[[0,0,0],[1,0,0,0]];
    VAR pose pse4:=[[0,0,0],[1,0,0,0]];
    VAR pose pse5:=[[0,0,0],[1,0,0,0]];

    pse2.rot:=twobj.uframe.rot;
    pse3.rot:=pnt.rot;
    pse4:=PoseInv(pse3);
    pse5:=PoseMult(pse4,pse2);
    ttool.tframe.rot:=pse5.rot;
  ENDPROC

  !**************************************************************
  ! Rutine name :CheckTCP
  ! Description :Checks TCP and compare with original TCP. Used in Auto.
  !**************************************************************
  PROC CheckTCP(
    INOUT tooldata tool, tooldata nomtool,
    gunnum setupNo
    \switch QuickCheck
    \bool Tol)

    measError:=FALSE;
    firstCheck:=TRUE;
    
    WaitTime\inpos,0;
    wCurrWobj:=CWObj();
    tempRobt:=pInitial{setupNo};
  
    IF StandardGun THEN
      tempRobt_ExtB_Check:=CRobT(\Tool:=tool);   
      tempRobt_ExtB_Check.extax.eax_b:=TipWearSetUpGunOpening{setupNo};
      MoveL tempRobt_ExtB_Check,v200,Fine,tool\WObj:=wCurrWobj;
      
      CheckTCP2 tool,nomtool,setupNo\QuickCheck?QuickCheck\Tol?Tol;
    ELSE
      CheckTCP2 tool,nomtool,setupNo\QuickCheck?QuickCheck\Tol?Tol;
    ENDIF
    RETURN;
  ERROR
    	
    RETRY;
  ENDPROC

  PROC CheckTCP2(
    INOUT tooldata tool, tooldata nomtool,
    gunnum setupNo
    \switch QuickCheck
    \bool Tol)

    VAR num mounted_tool:=0;
    VAR num key_in:=0;
    VAR bool TolError;
    VAR bool TolWarning;
    VAR string txtX;
    VAR string txtY;
    VAR string txtZ;
    VAR string E_TXT_BUPDA{2};
    VAR robtarget tmpRobT;
    VAR bool updateToolhome:=FALSE;
    VAR bool Timeout;
    VAR bool loop:=TRUE;
    VAR bool quick:=TRUE;
    VAR string actionTXT;
    VAR string ToolName;

    define:=FALSE;
    Tol0.tload:=tool.tload;
    tDay1User{setupNo}:=nomtool;
    WaitTime\InPos,0.01;
    TPErase;
    tmpwobjBeam:=obThisGun{setupNo};
    SetSysData Tol0;
    SetSysData tmpwobjBeam;
    RAISE noErrPosTest;
    PosTest pInitial{setupNo},chTcpPosTol,100,Tol0,tmpwobjBeam;
    IF ERRNO<>noErrPosTest THEN
      tmpRobT:=CRobT(\Tool:=Tol0\WObj:=tmpwobjBeam);
      SetSysData tool;
      SetSysData wobj0;
      IF Distance(tmpRobT.trans,pInitial{setupNo}.trans)<2 THEN
        strCheckTCPStartPosError{6}:="   ";
        strCheckTCPStartPosError{17}:="   ";
        strCheckTCPStartPosError{7}:="   "+T_TXT_STPOS{Language};
        strCheckTCPStartPosError{18}:="   "+T_TXT_STPOS{Language};
      	strCheckTCPStartPosError{9}:="   ";
      	strCheckTCPStartPosError{20}:="   ";
        rErrDisplay 1, key_in,A_fpErrNo745CheckTCPStartPosErr,strCheckTCPStartPosError,stBuContinue_5;
        !TPErase;
        !TPWrite T_TXT_STPOS{Language};
        !TPReadFK key_in,"","","","","",T_TXT_CONT{Language};
      ELSE
        strCheckTCPStartPosError{6}:="   "+T_TXT_MOSTPOPS1{Language};
        strCheckTCPStartPosError{17}:="   "+T_TXT_MOSTPOPS1{Language};
        strCheckTCPStartPosError{7}:="   "+T_TXT_MOSTPOPS2{Language};
        strCheckTCPStartPosError{18}:="   "+T_TXT_MOSTPOPS2{Language};
      	strCheckTCPStartPosError{9}:="   ";
      	strCheckTCPStartPosError{20}:="   ";
        rErrDisplay 1, key_in,A_fpErrNo746CheckTCPStartPosErr,strCheckTCPStartPosError,stBuContinue_5;
        !TPErase;
        !TPWrite T_TXT_MOSTPOPS1{Language};
        !TPWrite T_TXT_MOSTPOPS2{Language};
        !TPReadFK key_in,"","","","","",T_TXT_CONT{Language};
      ENDIF
    ENDIF
    ConfL\Off;
    MoveL pInitial{setupNo},v50,fine,Tol0\WObj:=tmpwobjBeam;
    ConfL\On;
    WaitDI sen1,0\MaxTime:=1\TimeFlag:=Timeout;
    IF Timeout RAISE errBeamSensor;
    tmpRobT:=CRobT(\Tool:=tool\WObj:=wobj0);
    MoveJ tmpRobT,v100,fine,tool\WObj:=wobj0;
    
    GetSysData tool\ObjectName:=tNameBeam;
    Beamtool:=tool;
    tmptoolBeam:=Beamtool;
    IF firstCheck THEN
      IF Present(QuickCheck) THEN      	
      	IF nPlusYRot{setupNo}<24 OR nMinusYRot{setupNo}>-24 THEN
      	  ToolName:=ArgName(tool);	
          ErrWrite\W,"[Errwrite;Info;]",T_TXT_NO24DEG{Language}\RL2:="[;;"+ToolName+"]"\RL3:="[;;]";
      	ELSE  		
          Quick:=QuickChecking((tDay1{setupNo}),setupNo);
          MoveL pInitial{setupNo},v200,fine,Tol0\WObj:=tmpwobjBeam;
          IF Quick actionTXT:="QuickCheck OK";
          LogTcp setupNo,actionTXT;
          IF Quick RETURN;
        ENDIF  
      ENDIF 
    ENDIF
    Beamtool:=BEAM_GetNewTCP(Beamtool,setupNo);
    IF bDispOffset{setupNo} CalcDispOffSet Beamtool,setupNo;
    MoveL pInitial{setupNo},v50,fine,Tol0\WObj:=tmpwobjBeam;
    Beamtool.tframe.rot:=tmptoolBeam.tframe.rot;
    CalcOffSet Beamtool,setupNo;
    vKoord tool,diffXBeam,diffYBeam,diffZBeam;
    zTol:=FALSE;
    IF Present(Tol) THEN
      IF NOT Tol zTol:=TRUE;
    ENDIF
    DefTol zTol;
    TolError:=FALSE;
    IF diffXBeam>tXxTol{setupNo} OR diffXBeam<-tXxTol{setupNo} TolError:=TRUE;
    IF diffYBeam>tXyTol{setupNo} OR diffYBeam<-tXyTol{setupNo} TolError:=TRUE;
    IF zTol THEN
      IF diffZBeam>tXzTol{setupNo} OR diffZBeam<-tXzWTool{setupNo} TolError:=TRUE;
    ELSE
      IF diffZBeam>tXzTol{setupNo} OR diffZBeam<-tXzTol{setupNo} TolError:=TRUE;
    ENDIF
    actionTXT:=T_TXT_OK;
    measError:=FALSE;
    IF TolError OR OpMode()<>OP_AUTO THEN
      txtX:=NumToStr(diffXBeam,1);
      txtY:=NumToStr(diffYBeam,1);
      txtZ:=NumToStr(diffZBeam,1);
      WHILE loop DO
        loop:=FALSE;
        TPErase;
        IF firstCheck AND (OpMode()=OP_AUTO OR TolError) THEN
          key_in:=4;
          actionTXT:=T_TXT_AUTORETRY{Language};
          firstCheck:=FALSE;
          txtXFirstCh:=txtX;
          txtYFirstCh:=txtY;
          txtZFirstCh:=txtZ;
          diffX1:=diffXBeam;
          diffY1:=diffYBeam;
          diffZ1:=diffZBeam;
          LogTcp setupNo,actionTXT;
        ELSE
          IF NOT TolError TPWrite T_TXT_TCPOK{Language};
          TPWrite tNameBeam+E_TXT_BTOCHLC{Language};
          IF TolError OR (NOT firstCheck) THEN
            diffX2:=diffXBeam;
            diffY2:=diffYBeam;
            diffZ2:=diffZBeam;
            strCheckTCPError{6}:="   "+T_TXT_MEAS1{Language};
            strCheckTCPError{17}:="   "+T_TXT_MEAS1{Language};
            strCheckTCPError{7}:="   "+E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtXFirstCh,6)+" Y:"+RightShiftString(txtYFirstCh,6)+" Z:"+RightShiftString(txtZFirstCh,6)+" mm";
            strCheckTCPError{18}:="   "+E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtXFirstCh,6)+" Y:"+RightShiftString(txtYFirstCh,6)+" Z:"+RightShiftString(txtZFirstCh,6)+" mm";
            
            strCheckTCPError{8}:="   "+T_TXT_MEAS2{Language};
            strCheckTCPError{19}:="   "+T_TXT_MEAS2{Language};
            strCheckTCPError{9}:="   "+E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtX,6)+" Y:"+RightShiftString(txtY,6)+" Z:"+RightShiftString(txtZ,6)+" mm";
            strCheckTCPError{20}:="   "+E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtX,6)+" Y:"+RightShiftString(txtY,6)+" Z:"+RightShiftString(txtZ,6)+" mm";

!            TPWrite T_TXT_MEAS1{Language};
!            TPWrite E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtXFirstCh,6)+" Y:"+RightShiftString(txtYFirstCh,6)+" Z:"+RightShiftString(txtZFirstCh,6)+" mm";      
!            TPWrite T_TXT_MEAS2{Language};            
!            TPWrite E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtX,6)+" Y:"+RightShiftString(txtY,6)+" Z:"+RightShiftString(txtZ,6)+" mm";      
!            
!            TPWrite E_TXT_BERRINX{Language}+txtXFirstCh+"   "+txtX;
!            TPWrite E_TXT_BERRINY{Language}+txtYFirstCh+"   "+txtY;
!            TPWrite E_TXT_BERRINZ{Language}+txtZFirstCh+"   "+txtZ;
            diffX:=diffX1-diffX2;
            diffY:=diffY1-diffY2;
            diffZ:=diffZ1-diffZ2;
            IF (diffX>chTcpMaxDiff OR diffX<-chTcpMaxDiff) OR (diffY>chTcpMaxDiff OR diffY<-chTcpMaxDiff) OR (diffZ>chTcpMaxDiff OR diffZ<-chTcpMaxDiff) THEN
              actionTXT:=T_TXT_MEASERR{Language};
              LogTcp setupNo,actionTXT;
              E_TXT_BUPDA{Language}:=E_TXT_B002{Language}+NumToStr(chTcpMaxDiff,1);
              strCheckTCPError{10}:="   "+E_TXT_B002{Language}+NumToStr(chTcpMaxDiff,1);
              strCheckTCPError{21}:="   "+E_TXT_B002{Language}+NumToStr(chTcpMaxDiff,1);          
            ELSE
              actionTXT:="";
              LogTcp setupNo,actionTXT;
              E_TXT_BUPDA{Language}:=E_TXT_B001{Language};
              strCheckTCPError{10}:="   "+E_TXT_B001{Language};
              strCheckTCPError{21}:="   "+E_TXT_B001{Language};
            ENDIF
          ELSE
            strCheckTCPError{6}:="   ";
            strCheckTCPError{17}:="   ";
            strCheckTCPError{7}:="   "+E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtX,6)+" Y:"+RightShiftString(txtY,6)+" Z:"+RightShiftString(txtZ,6)+" mm";
            strCheckTCPError{18}:="   "+E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtX,6)+" Y:"+RightShiftString(txtY,6)+" Z:"+RightShiftString(txtZ,6)+" mm";
            
            strCheckTCPError{8}:="   ";
            strCheckTCPError{19}:="   ";
            strCheckTCPError{9}:="   ";
            strCheckTCPError{20}:="   ";


!            TPWrite E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtX,6)+" Y:"+RightShiftString(txtY,6)+" Z:"+RightShiftString(txtZ,6)+" mm";      
 	
!            TPWrite E_TXT_BERRINX{Language}+txtX;
!            TPWrite E_TXT_BERRINY{Language}+txtY;
!            TPWrite E_TXT_BERRINZ{Language}+txtZ;
            actionTXT:="";
            LogTcp setupNo,actionTXT;
            E_TXT_BUPDA{Language}:=E_TXT_BUPDAQ{Language};
            strCheckTCPError{10}:="   "+E_TXT_BUPDAQ{Language};
            strCheckTCPError{21}:="   "+E_TXT_BUPDAQ{Language};
          ENDIF
          IF E_TXT_BUPDA{Language}=E_TXT_B001{Language} rErrDisplay 1, key_in,A_fpErrNo747MeaslimitErr,strCheckTCPError,stBuUpd1_Sk2_SkStop3_Retry4_TOL5;
          IF E_TXT_BUPDA{Language}=E_TXT_B002{Language}+NumToStr(chTcpMaxDiff,1) rErrDisplay 1, key_in,A_fpErrNo748MeaslimitErr,strCheckTCPError,stBuSk2_SkStop3_Retry4_TOL5;
          IF E_TXT_BUPDA{Language}=E_TXT_BUPDAQ{Language} rErrDisplay 1, key_in,A_fpErrNo749MeaslimitErr,strCheckTCPError,stBuUpd1_Sk2_SkStop3_Retry4_TOL5;

!          IF E_TXT_BUPDA{Language}=E_TXT_B001{Language} ErrDisplay A_BeamFault,E_TXT_BUPDA{Language},key_in\text1:=T_TXT_UPDATE{Language}\text2:=T_TXT_SKIP\text3:=T_TXT_SKIPSTOP{Language}\text4:=T_TXT_RETRY\text5:=T_TXT_TOL{Language};
!          IF E_TXT_BUPDA{Language}=E_TXT_B002{Language}+NumToStr(chTcpMaxDiff,1) ErrDisplay A_BeamFault,E_TXT_BUPDA{Language},key_in\text2:=T_TXT_SKIP\text3:=T_TXT_SKIPSTOP{Language}\text4:=T_TXT_RETRY\text5:=T_TXT_TOL{Language};
!          IF E_TXT_BUPDA{Language}=E_TXT_BUPDAQ{Language} ErrDisplay A_BeamFault,E_TXT_BUPDA{Language},key_in\text1:=T_TXT_UPDATE{Language}\text2:=T_TXT_SKIP\text3:=T_TXT_SKIPSTOP{Language}\text4:=T_TXT_RETRY\text5:=T_TXT_TOL{Language};
          IF actionTXT=T_TXT_MEASERR{Language} measError:=TRUE;
          actionTXT:=stBuUpd1_Sk2_SkStop3_Retry4_TOL5 {key_in};
          LoggTCP2 actionTXT;
          IF key_in=5 THEN
            TPErase;
            TPWrite T_TXT_TOLCHTCP{Language};
            TPWrite "X +/- "\Num:=tXxTol{setupNo};
            TPWrite "Y +/- "\Num:=tXyTol{setupNo};
            TPWrite "Z +   "\Num:=tXzTol{setupNo};
            IF zTol THEN
              TPWrite "Z -   "\Num:=tXzWTool{setupNo};
            ELSE
              TPWrite "Z -   "\Num:=tXzTol{setupNo};
            ENDIF
            TPReadFK key_in,"","","","","",T_TXT_OK;
            loop:=TRUE;
          ELSE
            firstCheck:=TRUE;
          ENDIF
        ENDIF
      ENDWHILE
      IF key_in=1 THEN
        vKoord tDay1User{setupNo},diffXD1,diffYD1,diffZD1;
        TolWarning:=FALSE;
        IF diffXD1>tXD1Tol{setupNo} OR diffXD1<-tXD1Tol{setupNo} TolWarning:=TRUE;
        IF diffYD1>tXD1Tol{setupNo} OR diffYD1<-tXD1Tol{setupNo} TolWarning:=TRUE;
        IF diffZD1>tXD1Tol{setupNo} OR diffZD1<-tXD1Tol{setupNo} TolWarning:=TRUE;
        IF TolWarning THEN
          txtX:=NumToStr(diffXD1,1);
          txtY:=NumToStr(diffYD1,1);
          txtZ:=NumToStr(diffZD1,1);
          TPErase;
          TPWrite T_TXT_BWARNING{Language};
          TPWrite tNameBeam+E_TXT_BCHNOM1{Language};
          TPWrite E_TXT_BERRIN{Language}+"X:"+RightShiftString(txtX,6)+" Y:"+RightShiftString(txtY,6)+" Z:"+RightShiftString(txtZ,6)+" mm";      

!          TPWrite txtX;
!          TPWrite txtY;
!          TPWrite txtZ;
          ! Align the gun or change it
          TPWrite T_TXT_ALIGN{Language};
          ! Run the TCP-calibration again.
          TPWrite T_TXT_RUNCALIB{Language};
          ! BREAK, no change of TCP value is done
          TPReadFK key_in,T_TXT_BREAKNOCHA{Language},"","","BREAK","","";

!          TPReadFK key_in,E_TXT_BUPDAQ{Language},"","BREAK","","","";
          IF key_in=1 ErrWrite\W,"CheckTCP: ",tNameBeam+E_TXT_BUPDADY1{Language}\RL2:=txtX\RL3:=txtY\RL4:=txtZ;
        ENDIF
      ENDIF
    ELSE
      firstCheck:=TRUE;
    ENDIF
    IF actionTXT=T_TXT_OK THEN
      LogTcp setupNo,actionTXT;
      pPlusYRotNoTilt:=pPlusYRotNoTiltL;
      pMinusYRotNoTilt:=pMinusYRotNoTitL;
      pFaceNoTilt:=pFaceNoTiltLaRun;
    ENDIF      
    IF key_in=1 THEN
      Beamtool.tframe.trans.x:=Trunc(Beamtool.tframe.trans.x\Dec:=2);
      Beamtool.tframe.trans.y:=Trunc(Beamtool.tframe.trans.y\Dec:=2);
      Beamtool.tframe.trans.z:=Trunc(Beamtool.tframe.trans.z\Dec:=2);
      tool:=Beamtool;
      !      IF updateToolhome toolhome:=tool;
      tBeamLastRunUser{setupNo}:=tool;
      ErrWrite\W,"CheckTCP: ",tNameBeam+E_TXT_BUPDA{Language}\RL2:=txtX\RL3:=txtY\RL4:=txtZ;
      pPlusYRotNoTilt:=pPlusYRotNoTiltL;
      pMinusYRotNoTilt:=pMinusYRotNoTitL;
      pFaceNoTilt:=pFaceNoTiltLaRun;
     !      IF updateToolhome toolhome:=tool;
    ENDIF
    IF key_in=4 RAISE errRetry;
    firstCheck:=TRUE;
    SetSysData tool;
    SetSysData wobj0;
    !SendAlarm A_BeamFault,""\Reset;
    IF key_in=3 Stop;
  ERROR
    IF ERRNO=errRetry RAISE;
    IF ERRNO=noErrPosTest TRYNEXT;
    IF ERRNO=errBeamSensor THEN
      TPErase;
      TPWrite E_TXT_R1{Language};
      TPWrite E_TXT_R2{Language};
      TPWrite E_TXT_R3{Language};
      TPWrite E_TXT_R4{Language};
      TPWrite E_TXT_R5{Language};
      TPReadFK key_in,T_TXT_BBOK{Language},"","","","",T_TXT_OK;
      Stop\NoRegain;
      pInitial{setupNo}:=CRobT(\Tool:=Tol0\WObj:=tmpwobjBeam);
      RAISE;
    ENDIF
    RETRY;
  ENDPROC

  !**************************************************************
  PROC LogTcp(
    gunnum setupNo,
    string actionTXT)

    VAR num key_in:=0;

    Incr logCounterBeam;
    IF logCounterBeam>maxLogCounter THEN
      IF IsFile(file2Beam) RemoveFile "HOME:/"+file2Beam;
      IF IsFile(file1Beam) RenameFile "HOME:/"+file1Beam,"HOME:/"+file2Beam;
      logCounterBeam:=1;
    ENDIF
    Open "HOME:"\File:=file1Beam,file\Append;
    Write file," ";
    Write file,CDate()\NoNewLine;
    Write file,", "+CTime()\NoNewLine;
    Write file,", "+tNameBeam\NoNewLine;
    Write file,", "\Num:=setupNo\NoNewLine;
    Write file,", ToolDiff=, "\Num:=diffXBeam\NoNewLine;
    Write file,", "\Num:=diffYBeam\NoNewLine;
    Write file,", "\Num:=diffZBeam\NoNewLine;
    Write file,", ToolTol=, "\Num:=tXxTol{setupNo}\NoNewLine;
    Write file,", "\Num:=tXyTol{setupNo}\NoNewLine;
    Write file,", "\Num:=tXzTol{setupNo}\NoNewLine;
    IF zTol THEN
      Write file,", -z=, "\Num:=tXzWTool{setupNo}\NoNewLine;
    ELSE
      Write file,", -z=, "\Num:=tXzTol{setupNo}\NoNewLine;
    ENDIF
    Write file,", Old TCP=, "\Num:=Trunc(tmptoolBeam.tframe.trans.x\Dec:=2)\NoNewLine;
    Write file,", "\Num:=Trunc(tmptoolBeam.tframe.trans.y\Dec:=2)\NoNewLine;
    Write file,", "\Num:=Trunc(tmptoolBeam.tframe.trans.z\Dec:=2)\NoNewLine;
    IF actionTXT=T_TXT_OK OR actionTXT=T_TXT_AUTORETRY{Language} OR actionTXT=T_TXT_MEASERR{Language} THEN
      Write file,",,,,"\NoNewLine;
    ENDIF
    IF actionTXT=T_TXT_OK OR actionTXT=T_TXT_AUTORETRY{Language} OR actionTXT=T_TXT_MEASERR{Language} Write file,", Action="\NoNewLine;
    IF actionTXT=T_TXT_OK OR actionTXT=T_TXT_AUTORETRY{Language} OR actionTXT=T_TXT_MEASERR{Language} Write file,", "+actionTXT\NoNewLine;
    Close file;
    rSendAlarm A_BEAM_LoggwriteError," "\Reset;
  ERROR
    IF ERRNO=ERR_FILEOPEN THEN
!      TPWrite T_TXT_DISABLELOG{language};
!      TPWrite T_TXT_RETRY+":";
      rErrDisplay 1, key_in,A_BEAM_LoggwriteError,strSwErrLogg1RAMDiskErr,stBuDisableRetry_3_5;
 !     ErrDisplay A_OtherRobFault,E_TXT_RAMERR{language},key_in\text3:="DISABLE"\text5:="RETRY";
      TEST key_in
      CASE 3:
        disableLogWrite:=TRUE;
        rSendAlarm A_BEAM_LoggwriteError," "\Reset;
        RETURN;
      CASE 5:
        RETRY;
      ENDTEST
    ENDIF
    IF ERRNO=ERR_FILEACC THEN
      TRYNEXT;
    ENDIF
  ENDPROC

  ! ************************************************************************************
  PROC LoggTCP2(
    string action)

    VAR num key_in:=0;
    VAR bool tempBool;

    Open "HOME:"\File:=file1Beam,file\Append;
    IF action=T_TXT_UPDATE{Language} THEN
      Write file,", New TCP=, "\Num:=Trunc(Beamtool.tframe.trans.x\Dec:=2)\NoNewLine;
      Write file,", "\Num:=Trunc(Beamtool.tframe.trans.y\Dec:=2)\NoNewLine;
      Write file,", "\Num:=Trunc(Beamtool.tframe.trans.z\Dec:=2)\NoNewLine;
      Write file,", "\NoNewLine;
      Write file," Action=,"\NoNewLine;
    ELSE
      IF NOT measError AND action<>T_TXT_OK Write file,",,,,,"\NoNewLine;
      IF NOT measError AND action<>T_TXT_OK Write file," Action=,"\NoNewLine;
    ENDIF
    IF action<>T_TXT_OK Write file," "+action\NoNewLine;
    Close file;
    !SendAlarm A_BeamFault,""\Reset;
  ERROR
    IF ERRNO=ERR_FILEOPEN THEN
!      TPWrite T_TXT_DISABLELOG{language};
!      TPWrite T_TXT_RETRY+":";
      rErrDisplay 1, key_in,A_BEAM_LoggwriteError,strSwErrLogg1RAMDiskErr,stBuDisableRetry_3_5;
 !     ErrDisplay A_OtherRobFault,E_TXT_RAMERR{language},key_in\text3:="DISABLE"\text5:="RETRY";
      TEST key_in
      CASE 3:
        disableLogWrite:=TRUE;
        rSendAlarm A_BEAM_LoggwriteError," "\Reset;
        RETURN;
      CASE 5:
        RETRY;
      ENDTEST
    ENDIF
    IF ERRNO=ERR_FILEACC THEN
      TRYNEXT;
    ENDIF
  ENDPROC

  !**************************************************************
  LOCAL PROC DefTol(
    bool Tol)

    tXxTol{1}:=t1xTol;
    tXxTol{2}:=t2xTol;
    tXxTol{3}:=t3xTol;
    tXxTol{4}:=t4xTol;
    tXxTol{5}:=t5xTol;

    tXyTol{1}:=t1yTol;
    tXyTol{2}:=t2yTol;
    tXyTol{3}:=t3yTol;
    tXyTol{4}:=t4yTol;
    tXyTol{5}:=t5yTol;

    tXzTol{1}:=t1zTol;
    tXzTol{2}:=t2zTol;
    tXzTol{3}:=t3zTol;
    tXzTol{4}:=t4zTol;
    tXzTol{5}:=t5zTol;

    tXxQTol{1}:=t1xQTol;
    tXxQTol{2}:=t2xQTol;
    tXxQTol{3}:=t3xQTol;
    tXxQTol{4}:=t4xQTol;
    tXxQTol{5}:=t5xQTol;

    tXyQTol{1}:=t1yQTol;
    tXyQTol{2}:=t2yQTol;
    tXyQTol{3}:=t3yQTol;
    tXyQTol{4}:=t4yQTol;
    tXyQTol{5}:=t5yQTol;

    tXzQTol{1}:=t1zQTol;
    tXzQTol{2}:=t2zQTol;
    tXzQTol{3}:=t3zQTol;
    tXzQTol{4}:=t4zQTol;
    tXzQTol{5}:=t5zQTol;


    tXzWTool{1}:=0-t1zWeartool;
    tXzWTool{2}:=0-t2zWeartool;
    tXzWTool{3}:=0-t3zWeartool;
    tXzWTool{4}:=0-t4zWeartool;
    tXzWTool{5}:=0-t5zWeartool;

    tXD1Tol{1}:=t1Day1Tol;
    tXD1Tol{2}:=t2Day1Tol;
    tXD1Tol{3}:=t3Day1Tol;
    tXD1Tol{4}:=t4Day1Tol;
    tXD1Tol{5}:=t5Day1Tol;

  ENDPROC

  !**************************************************************
  LOCAL PROC vKoord(
    tooldata tool,
    INOUT num vkx,
    INOUT num vky,
    INOUT num vkz)

    VAR pose tempPose:=[[0,0,0],[1,0,0,0]];
    nyPoseBeam.trans:=Beamtool.tframe.trans;
    inPoseBeam.trans:=tool.tframe.trans;
    inPoseBeam.rot:=tool.tframe.rot;
    tempPose:=PoseMult(PoseInv(inPoseBeam),nyPoseBeam);
    vkx:=Round(tempPose.trans.x\Dec:=1);
    vky:=Round(tempPose.trans.y\Dec:=1);
    vkz:=Round(tempPose.trans.z\Dec:=1);
ENDPROC

  !**************************************************************
  LOCAL PROC CalcOffSet(
    INOUT tooldata intool,
    gunnum setupNo)

    VAR num key_in:=0;
    VAR pose tempPose:=[[0,0,0],[1,0,0,0]];
    VAR num offsetX:=0;
    VAR num offsetZ:=0;

    IF define THEN
      TPReadNum offsetX,T_TXT_BOFFX{Language};
      TPReadNum offsetZ,T_TXT_BOFFZ{Language};
      poseOffset{setupNo}.trans:=[offsetX,0,offsetZ];
    ENDIF
    tempPose.trans:=intool.tframe.trans;
    tempPose.rot:=intool.tframe.rot;
    poseReadyOffset:=PoseMult(tempPose,poseOffset{setupNo});
    intool.tframe.trans:=poseReadyOffset.trans;
  ERROR
    RETRY;
  ENDPROC


  !**************************************************************
  LOCAL PROC CalcDispOffSet(
    INOUT tooldata intool,
    gunnum setupNo)

    VAR num key_in:=0;
    VAR pose tempPose:=[[0,0,0],[1,0,0,0]];
    VAR num offsetX:=0;
    VAR num offsetZ:=0;


    intool.tframe.trans.x:=intool.tframe.trans.x+psOffsetDisp{setupNo}.x;
    intool.tframe.trans.y:=intool.tframe.trans.y+psOffsetDisp{setupNo}.y;
    intool.tframe.trans.z:=intool.tframe.trans.z+psOffsetDisp{setupNo}.z;
    bDispOffset{setupNo}:=TRUE;
  ERROR
    RETRY;
  ENDPROC

  !**************************************************************
  PROC ShowTcpFault(
    INOUT tooldata InTool)

    VAR num key_in:=0;
    VAR robtarget act_pos;
    VAR string txtX;
    VAR string txtY;
    VAR string txtZ;
    VAR string InToolName;
    VAR bool repeat:=TRUE;
    VAR bool startPos:=TRUE;

    ! Check that tool argument = tNameBeam (last run with CheckTCP)
    InToolName:=ArgName(InTool);
    IF tNameBeam<>InToolName THEN
      TPErase;
      TPWrite TXT_TOOL_ERR1{Language};
      TPWrite TXT_TOOL_ERR2{Language};
      TPReadFK key_in,TXT_TOOL_ERR3{Language},"","","","",T_TXT_OK;
      Stop;
      RETURN;
    ENDIF
    ! Record current position
    WaitTime\InPos,0.1;
    act_pos:=CRobT(\Tool:=InTool\WObj:=wobj0);
    ! Deviations from last CheckTCP
    txtX:=E_TXT_BERRINX{Language}+NumToStr(diffXBeam,1);
    txtY:=E_TXT_BERRINY{Language}+NumToStr(diffYBeam,1);
    txtZ:=E_TXT_BERRINZ{Language}+NumToStr(diffZBeam,1);
    repeat:=TRUE;
    WHILE repeat DO
      TPErase;
      TPWrite TXT_MEAS_DEV{Language}+InToolName;
      TPWrite txtX;
      TPWrite txtY;
      TPWrite txtZ;
      IF startPos THEN
        TPReadFK key_in,TXT_SHOW_DEV{Language},T_TXT_MOVE{language},"","","",T_TXT_SKIP;
      ELSE
        TPReadFK key_in,TXT_SHOW_CURR{Language},"",T_TXT_MOVE{language},"","",T_TXT_SKIP;
      ENDIF
      TEST key_in
      CASE 1:
        startPos:=FALSE;
        MoveL RelTool(act_pos,diffXBeam,diffYBeam,diffZBeam),v5,fine,InTool\WObj:=wobj0;
      CASE 2:
        MoveL RelTool(act_pos,0,0,0),v5,fine,InTool\WObj:=wobj0;
        startPos:=TRUE;
      DEFAULT:
        ! ABORT
        repeat:=FALSE;
      ENDTEST
    ENDWHILE
    Stop;
  ERROR
    RETRY;
  ENDPROC

  ! ************************************************************************************
  ! Routines for test if robot is in the ordered position and have correct direction
  PROC PosTest(
    robtarget testPosition
    \robtarget byPosition,
    num posTolerance,
    num rotTolerance,
    INOUT tooldata Tool,
    INOUT wobjdata Wobj)

    VAR string txtDistance:="";
    VAR num key_in:=0;
    VAR speeddata spmove:=v40;
    VAR robtarget currentPosition;
    VAR string txtDeviation:="";
    VAR num angleDeviation:=0;
    VAR num axisMaxDevation:=1;
    VAR string txtAxisNo:="";
    VAR AlarmNo intAlarmNo;


    WaitTime\InPos,0.01;
!    IF CWObj()<>Wobj TPWrite M_TXT_CURWOBJ;
    currentPosition:=CRobT(\Tool:=Tool\WObj:=Wobj);
    IF Distance(currentPosition.trans,testPosition.trans)>posTolerance RAISE err_pos;
    AxisConfCheck currentPosition,testPosition,Tool,Wobj,angleDeviation,axisMaxDevation;
    IF angleDeviation>rotTolerance THEN
      RAISE err_rot;
    ELSE
      rSendAlarm intAlarmNo," "\Reset;	
      !SendAlarm A_RobotNotOnPath,""\Reset;
    ENDIF
  ERROR
    IF ERRNO=err_pos THEN
      !TPWrite E_POS_TOL{language};
      txtDistance:=NumToStr(Distance(currentPosition.trans,testPosition.trans),0);
      !TPWrite M_TXT_OFFDIST{language}+txtDistance+" mm";
      strBeamPosTestMoveByError{6}:="   "+M_TXT_OFFDIST{language}+txtDistance+" mm";
      strBeamPosTestMoveByError{17}:="   "+M_TXT_OFFDIST{language}+txtDistance+" mm";
      strBeamPosTestMoveDirError{6}:="   "+M_TXT_OFFDIST{language}+txtDistance+" mm";
      strBeamPosTestMoveDirError{17}:="   "+M_TXT_OFFDIST{language}+txtDistance+" mm";
      IF Present(byPosition) THEN
      	intAlarmNo:=A_fpErrNo750CheckTCPStartPosErr;
      	rErrDisplay 1, key_in,A_fpErrNo750CheckTCPStartPosErr,strBeamPosTestMoveByError,stBuMoveSkip_1_2;
        !ErrDisplay A_RobotNotOnPath,M_TXT_MOVEBY{language},key_in\text1:=T_TXT_MOVE{language}\text2:=T_TXT_SKIP;
      ELSE
      	intAlarmNo:=A_fpErrNo751CheckTCPStartPosErr;
      	rErrDisplay 1, key_in,A_fpErrNo751CheckTCPStartPosErr,strBeamPosTestMoveDirError,stBuMoveSkip_1_2;
        !ErrDisplay A_RobotNotOnPath,M_TXT_MOVEDIR{language},key_in\text1:=T_TXT_MOVE{language}\text2:=T_TXT_SKIP;
      ENDIF
      TEST key_in
      CASE 1:
        TPReadFK key_in,M_TXT_MOVESPEED{language},"v100","v40",T_TXT_SKIP,"","";
        TEST key_in
        CASE 1:
          spmove:=v100;
        CASE 2:
          spmove:=v40;
        CASE 3:
          rSendAlarm intAlarmNo," "\Reset;
          !SendAlarm A_RobotNotOnPath,""\Reset;
          RETURN;
        ENDTEST
        IF Present(byPosition) THEN
          MoveJ byPosition,spmove,fine,Tool\WObj:=Wobj;
          MoveL testPosition,spmove,fine,Tool\WObj:=Wobj;
        ELSE
          MoveJ testPosition,spmove,fine,Tool\WObj:=Wobj;
        ENDIF
        WaitTime 0.3;
        RAISE;
      CASE 2:
        RETURN;
      DEFAULT:
        TPWrite E_TXT_EXEC{language};
        Stop;
      ENDTEST
      RETURN;
    ENDIF
    IF ERRNO=err_rot THEN
      !TPWrite E_ROT_TOL{language};
      txtDeviation:=NumToStr(angleDeviation,1);
      txtAxisNo:=NumToStr(axisMaxDevation,0);
      !TPWrite M_TXT_OFFROT{language}+txtAxisNo+": "+txtDeviation+" deg";
      strBeamPosRotTestMoveByError{6}:="   "+M_TXT_OFFROT{language}+txtAxisNo+": "+txtDeviation+" deg";
      strBeamPosRotTestMoveByError{17}:="   "+M_TXT_OFFROT{language}+txtAxisNo+": "+txtDeviation+" deg";
      strBeamPosRotTestMoveDirError{6}:="   "+M_TXT_OFFROT{language}+txtAxisNo+": "+txtDeviation+" deg";
      strBeamPosRotTestMoveDirError{17}:="   "+M_TXT_OFFROT{language}+txtAxisNo+": "+txtDeviation+" deg";
      IF Present(byPosition) THEN
      	intAlarmNo:=A_fpErrNo752CheckTCPStartPosErr;
      	rErrDisplay 1, key_in,A_fpErrNo752CheckTCPStartPosErr,strBeamPosRotTestMoveByError,stBuMoveSkip_1_2;
        !ErrDisplay A_RobotNotOnPath,M_TXT_MOVEBY{language},key_in\text1:=T_TXT_MOVE{language}\text2:=T_TXT_SKIP;
      ELSE
      	intAlarmNo:=A_fpErrNo753CheckTCPStartPosErr;
      	rErrDisplay 1, key_in,A_fpErrNo753CheckTCPStartPosErr,strBeamPosRotTestMoveDirError,stBuMoveSkip_1_2;
        !ErrDisplay A_RobotNotOnPath,M_TXT_MOVEDIR{language},key_in\text1:=T_TXT_MOVE{language}\text2:=T_TXT_SKIP;
      ENDIF
      TEST key_in
      CASE 1:
        IF Present(byPosition) THEN
          spmove:=[40,75,200,15];
          MoveJ byPosition,spmove,fine,Tool\WObj:=Wobj;
          spmove:=v100;
          MoveL testPosition,spmove,fine,Tool\WObj:=Wobj;
          WaitTime 0.3;
        ENDIF
        RAISE;
      CASE 2:
        rSendAlarm intAlarmNo," "\Reset;
        !SendAlarm A_RobotNotOnPath,""\Reset;
        RETURN;
      DEFAULT:
        TPWrite E_TXT_EXEC{language};
        Stop;
      ENDTEST
      RETURN;
    ELSE
      TPWrite E_TXT_EXEC{language}\Num:=ERRNO;
      Stop;
    ENDIF
  ENDPROC

  ! ************************************************************************************
  ! Returns largest axis angle deviation inbetween configuration at reference and test robtarget
  LOCAL PROC AxisConfCheck(
    INOUT robtarget ref_point,
    INOUT robtarget test_point,
    INOUT tooldata Tool,
    INOUT wobjdata Wobj,
    INOUT num maxDiffAngle,
    INOUT num axisMaxDiffAngle)

    VAR num diffAngle:=0;
    VAR jointtarget jointRef;
    VAR jointtarget jointTest;

    jointRef:=CalcJointT(ref_point,Tool\WObj:=Wobj);
    jointTest:=CalcJointT(test_point,Tool\WObj:=Wobj);
    maxDiffAngle:=Abs(jointRef.robax.rax_1-jointTest.robax.rax_1);
    axisMaxDiffAngle:=1;
    diffAngle:=Abs(jointRef.robax.rax_2-jointTest.robax.rax_2);
    IF diffAngle>maxDiffAngle THEN
      maxDiffAngle:=diffAngle;
      axisMaxDiffAngle:=2;
    ENDIF
    diffAngle:=Abs(jointRef.robax.rax_3-jointTest.robax.rax_3);
    IF diffAngle>maxDiffAngle THEN
      maxDiffAngle:=diffAngle;
      axisMaxDiffAngle:=3;
    ENDIF
    diffAngle:=Abs(jointRef.robax.rax_4-jointTest.robax.rax_4);
    IF diffAngle>maxDiffAngle THEN
      maxDiffAngle:=diffAngle;
      axisMaxDiffAngle:=4;
    ENDIF
    diffAngle:=Abs(jointRef.robax.rax_5-jointTest.robax.rax_5);
    IF diffAngle>maxDiffAngle THEN
      maxDiffAngle:=diffAngle;
      axisMaxDiffAngle:=5;
    ENDIF
    diffAngle:=Abs(jointRef.robax.rax_6-jointTest.robax.rax_6);
    IF diffAngle>maxDiffAngle THEN
      maxDiffAngle:=diffAngle;
      axisMaxDiffAngle:=6;
    ENDIF
  ENDPROC

 ! ************************************************************************************
  LOCAL FUNC string RightShiftString(string stringIn,num maxLenght)
    VAR string blankFillIn:="";
    VAR string stringOut:="";
    VAR num stringLenght:=0;

    stringLenght:=StrLen(stringIn);
    IF stringLenght<maxLenght THEN
      FOR i FROM stringLenght+1 TO maxLenght DO
        blankFillIn:=blankFillIn+" ";
      ENDFOR
    ENDIF
    stringOut:=blankFillIn+stringIn;
    RETURN stringOut;
  ERROR
    RETRY;
  ENDFUNC

ENDMODULE
