     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *
     h dftactgrp(*no) actgrp('QILE')
     h debug(*yes)
     h option(*nodebugio: *srcstmt)
      *****************************************************************
      *TITLE SGETDII - STORED PROCEDURE - GET Dart Records for Grid   *
      * DESCRIPTION:                                                  *
      *      Called from Idarts - to load Diary grid
      *****************************************************************
      * REVISIONS
      *
      * 07/01/21 C:079 Support: 146/79753
      *                Add W0050Q to processing for Quote
      *
      * 01/11/17 C:136 PhaseWare: 45002
      *                GetEmpYrRR was returning zeros because the
      *                override was not being cleared if it was set
      *
      * 08/25/16 C:136 Support: 42863
      *                Rename WMCML1 fields to not overwrite
      *                SDCMPDS1 values
      *
      * 05/20/16  C:33   Elevate
      *                  -Retrieve RRN for SIS file(s).
      *
      * 03/30/16  C:33   Elevate
      *                  -Eliminate service program(s).
      *                  -Eliminate "team" file(s).
      *
      * 08/12/15  C:279  Support: 32903
      *                  Modified to check deallocated team member status.
      *
      * 06/26/14  C:300  Project DR312
      *                  DARTMAIL- Return Emailed flag
      *
      * 08/12/13  C:33   Redmime 3538
      *                  Change to use SDCMLUC instead of IAKNL1.
      *
      * 07/01/13  C:282  Redmime 3249
      *                  Recomile Only.
      *
      * 02/04/13  C:062  Project SY266
      *                  Deal with record overflows
      *
      * 11/14/12  C:062  Project SY266
      *                  Change to get the detail rrn
      *
      * 10/11/12  C:062  Project SY266
      *                  Restrict darts is date passed in
      *
      * 09/29/12  C:062  Project SY266
      *                  Pass the app RRN.
      *
      * 09/18/12  C:062  Project SY266
      *                  Add logic to return team info if indicated to do so
      *
      * 06/09/11  C:061  Project 133IKU
      *                  Populate 'Claim Adjuster' and 'Claim Status'
      *                  fields.  Change call to CR001R to get new field
      *                  values.
      *
     H*  02/18/11  C:33  Seq#: ID045/75700
     H*                  Remove SQL statement.
     H*
     H*   10/05/10 C:062 SUPPORT: 74398
     H*                  Don't Execute SMCLP query if addtlr='Y'.
     H*
     H*   10/06/09 C:220 SUPPORT: 70088
     H*                Increase in len of fields NUMATTACH to 5,0.
     H*                -Just compiled.
     H*
     H*   09/03/09 C:232 SUPPORT: 69600                               *
     H*                  idarts - Additional Remarks Field issue.     *
     H*
     H*   12/05/06 - India change - Added SMMD for this proc.
     H*
     ***========================================================================***
     **** Create Procdure must occur on the clients machine as this is installed****
     **     Below is the SQL used on client 129's Machine to "register" the stored proc.
      *
      *   CREATE PROCEDURE
      *   pdsobj/SGETDii(
      *   IN INUSEER CHAR(10),
      *      INCDAT  CHAR(7),
      *      INCTEAM CHAR(1))
      *
      *   RESULT SETS 1
      *
      *   LANGUAGE RPGLE
      *   EXTERNAL NAME
      *   pdsobj/SGETDii
      *   READS SQL DATA
      *   PARAMETER STYLE SQL
     ***========================================================================***
     H*****************************************************************
     H*  CREATED: 01/11/06                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   60    GENERAL CHAIN                                         *
     H*****************************************************************
     FSDCMLUC   IF   E           K DISK
     FSMARL     IF   E           K DISK
     FSTSML     IF   E           K DISK
     FSMKDL     IF   E           K DISK
     FSMMDL     IF   E           K DISK
     FSMTEL     IF   E           K DISK    ReName(pte01:pte00) usropn
     FSMTEL1    IF   E           K DISK                        usropn
     FSDTSL2    IF   E           K DISK    ReName(pts01:pts02) usropn
     FSDTSL3    IF   E           K DISK    ReName(pts01:pts03) usropn
     Fwdell     if   e           k disk    infds(elFDS)  usropn
     F                                     rename(pel01:wel00)
     Fwmcml1    if   e           k disk    infds(cmFDS1) usropn
     F                                     rename(pcm01:wcm01)
     f                                     prefix(#)
     d* Proto-types

     d*copy*wPrClaims
     d*copy*wPrEmplr

     d* Process Management Info Screen
     d BldTeamIdList   pr
     d manager                       10

     d* Check to see if this team id is not a root
     d ChkRootTeamId   pr              n
     d manager                       10
     d teamId                         5  0

     DGetClaimRRN      PR             9  0
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const

     d* Get Diary Count
     d GetDiaryCount   pr             5  0
     d user                          10
     d date                           7

     DGetEmpYrRR       PR             9  0
     D inCo#                               like(ELco#)  const
     D inGroup                             like(ELfnd)  const
     D inEmp#                              like(ELemp#) const
     D inDiv                               like(ELdiv)  const
     D inEnv                          1                 const

     DGetSisRRN        PR             9  0
     D key                           40    const
     D pgm                           10    const

     DRtvQuoteLib      pr            10
     D inEnv                          4    const

     DRtvEnviLib       pr
     D inEnv                         10    const
     D outLib                        10    const
     D outMLib                       10    const

     d* Write to Team User Result Set
     d WriteTeamUser   pr
     d teamId                         5  0
     d teamName                      20
     d parentTeamName                20
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     DSDCMP          E DS                  EXTNAME(SDCMPDS1) OCCURS(9999)
     DcmFDS1           ds
     D cmRRN1                397    400b 0
     DelFDS            ds
     D elRRN                 397    400b 0
     D*ARFLG
     D
     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*
     Di                s              5  0
     Dj                s              5  0
     D@count           s              5  0 inz
     D k               s              5  0
     D kkMgr           s                   Like(TEmgr)
     D kkTeamId        s                   like(TEid)
     D kkType          s                   like(TStype)
     D kkUsr           s                   like(TSusr)
     D parentTeam      s                   Like(TEname)
     D pos             s              5s 0
     D company         s              3  0
     D group           s              3  0
     D year            s              3  0
     D case            s              7  0
     D employer        s              9  0
     D division        s              5  0
     D yearFlag        s              1
     D env             s              1
     D kkCdat          s                   like(CMcDat)

     D teamIdList      s                   Like(TEid) DIM(9999)

     D teamListing     ds                  Dim(9999)
     D                                     Qualified
     D team                                Like(TEname)
     D parentTeam                          Like(TEname)
     D teamUser                            Like(TSusr)
     D diaryCount                     5s 0
      *
     C** DART FILE
     C     CMK           KLIST
     C                   KFLD                    INUSER
     C                   KFLD                    kkCdat
      *
     C     CMK1          KLIST
     C                   KFLD                    INUSER
      *
     C*  Add rmk file
     C     ark           KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
      *
     C*  Summ code file
     C     sck           KLIST
     C                   KFLD                    CMSMCD
     C*  Add mdk file
     C     mdk           KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     *******
     C*   KEY LIST FOR THE KEY DEFINITION FILE  *** ADDED TO GET NAME ****
     C*
     C     KDKEY         KLIST
     C                   KFLD                    QQPGM
     C*
     C* Team File Logicals
     C*
     C     keySMTEL      KLIST
     C                   KFLD                    kkTeamId
     C*
     C     keySMTEL1     KLIST
     C                   KFLD                    kkMgr
     C*
     C* Team Members Logical
     C*
     C     key1SDTSL2    KLIST
     C                   KFLD                    kkTeamId
     C*
     C     key2SDTSL2    KLIST
     C                   KFLD                    kkTeamId
     C                   KFLD                    kkType
     C                   KFLD                    kkUsr
     C*
     C     keySDTSL3     KLIST
     C                   KFLD                    kkType
     C                   KFLD                    kkUsr
      *
     ******
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ** Date check/conversion parameters
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     *ENTRY        PLIST
     C                   PARM                    INUSER           10
     C                   PARM                    INCD              7            7/0 date in char fmt
     C                   PARM                    inTeamFlag        1
      *
     C                   MOVE      INCD          INCDAT            7 0
      /free
       //
       // If Called to return team data
       //
       If inTeamFlag='Y';
          If not %open(SMTEL);
             Open SMTEL;
             Open SMTEL1;
             Open SDTSL2;
             Open SDTSL3;
          Endif;
          // Build Team Id List
          BldTeamIdList(inUser);
          i=0;
          // Read thru the root teams that this person is a manager of
          For k=1 to 9999;
             // If the value is 0 then there aren't any teams left to process
             If teamIdList(k)=0;
                Leave;
             // Process the team and all sub-teams
             Else;
                kkTeamId=teamIdList(k);
                Chain keySMTEL pte00;
                // Read thru all users in that team
                parentTeam=*Blanks;
                // Write to the array info for all team members
                writeTeamUser(TEid:TEname:parentTeam);
             EndIf;
          EndFor;
          //
          // Return Result Set
          Exec Sql set result sets with return to caller
           array :TEAMLISTING for :I rows;
       //
       // If the team flag is not equal to Y then process in the original way.
       Else;
      /end-free
     C*
     **** SET INDEX TO 0
     C                   EVAL      I=0
     C*
     C*** USE KEY TO "GET" ALL RECORDS THAT MEET CRITERIA
     C*** Read DARTS Remark file
     C                   If        incDat=0
     C                   eval      kkCdat=9999999
     C                   else
     C                   eval      kkCdat=incDat
     C                   EndIf
     C                   If        kkCdat=9999999
     C     CMK           SETGT     PCM01
     C     CMK1          readpe    PCM01                                  50    User only get
     C                   else
     C     CMK           SETGT     PCM01
     C     CMK           ReadPe    PCM01                                  50
     C                   Endif
     C     *in50         ifeq      *off                                          if hit
     C     CMCDAT        andle     kkCdat                                        and rcd date is le
     C** hit

     C*** INCREMENT INDEX
     C                   EVAL      I=I+1
     C*** SET OCCURANCE FOR DS
     C     I             OCCUR     SDCMP
     c                   else
     c                   goto      norcd
     c                   endif
      ****
     C     *IN50         DOWEQ     *OFF
     C     CMCDAT        andle     kkCdat                                        and rcd date is le
     C*
     C**
     *** Get AR file here
     c     ark           setll     par01                                  51
     C     *in51         ifeq      *on
     c*** hit on AR file.. set indicator in multioccur ds to pass back
     C                   eval      adlrmkf='Y'
     C                   else
     C                   eval      adlrmkf='N'
     C                   endif
     c*
     c                   if        adlrmkf='N'
      *
     c                   if        @count <> 0
     c                   eval      adlrmkf='Y'
     c                   else
     c                   eval      adlrmkf='N'
     c                   endif
      *
     c                   endif
     *** Get Summary Code description for output to SCDDESC
     c     sck           chain     psm01                              51
     C     *in51         ifeq      *off
     c*** hit on SM file.. set indicator in multioccur ds to pass back
     C                   eval      scddesc=smdesc
     C                   else
     C                   eval      scddesc=*blanks
     C                   endif
     ***add code to GET NAMES FOR RETURN
     *** REMOVED the GET names, as it was SLOW
     C                   EXSR      GETNAM
     ***
     c*
     *** Get Number of attachment in MDK file
     c                   Z-add     *zeros        NumAttach
     c     mdk           chain     pmd01                              51
     C     *in51         doweq     *off
     c                   Eval      NumAttach = NumAttach + 1
     c     mdk           reade     pmd01                                  51
     c                   enddo
     c*
     c* Assign Application Rrn
     c*
      /free
       Select;
       //*** Beg Chg *** 07/01/21 ********************************************
       //***When CMpNam='W0050R';
       When CMpNam='W0050R' or CMpNam = 'W0050Q';
       //*** End Chg *** 07/01/21 ********************************************
          year=0;
          company=%Dec(%Subst(CMkey:1:3):3:0);
          group=%Dec(%Subst(CMkey:4:3):3:0);
          employer=%Dec(%Subst(CMkey:7:9):9:0);
          division=%Dec(%Subst(CMkey:16:5):5:0);
          env=%Subst(CMkey:40:1);
          yearFlag=' ';
          appRrn=%EditC(getEmpYrRR(company:group:employer:division:env):'X');
       When CMpNam='W0070R';
          company=%Dec(%Subst(CMkey:1:3):3:0);
          group=%Dec(%Subst(CMkey:4:3):3:0);
          year=%Dec(%Subst(CMkey:7:3):3:0);
          case=%Dec(%Subst(CMkey:10:7):7:0);
          appRrn=%EditC(getClaimRrn(company:group:year:case):'X');
       When CMpNam='SISPOL' or CMpNam='SISCLM';
          appRrn=%EditC(GetSisRrn(CMkey:CMpNam):'X');
       Other;
          appRrn='000000000';
       EndSl;
      /end-free
     c*
      * Assign Emailed flag from SDCMP.CMEMAILFLG
      /free
          EMailed = CMemailflg;
      /end-free
     c                   eval      i=I+1
     C     I             OCCUR     SDCMP
     C                   If        i=9999
     C                   Clear                   pcm01
     C                   eval      CMseq#=-1
     C                   eval      CMcomm='The maximum number of records have' +
     C                             ' been reached.'
     C                   Leave
     C                   Endif
     C*
     C*** Read next DARTS Remark Record
     C                   If        kkCdat=9999999
     C     CMK1          readpe    PCM01                                  50
     C                   else
     C     CMK           readpe    PCM01                                  50
     C                   Endif
     c
     C                   ENDDO
     C*
     c     i             ifeq      0
     c                   goto      endit
     c                   else
     c                   eval      j=i-1
     c                   endif
     C*
     C     norcd         tag
     **----------------------- SQL area ---------------------------------------------------**
     C*
     c/exec sql
     c+   set result sets array :SDCMP   for :J   rows
     c/end-exec
      /free
       EndIf;
      /end-free
     C*
     C*** END PROGRAM
     C*
     c     endit         tag
     C                   MOVE      *ON           *INLR
     C*
     **=============================================================================================
     c     GETNAM        begsr
     **=============================================================================================
     **
     C*** INITIALIZE THE PARMS FOR PROGRAM CALL
     C*
     C                   MOVEL     *BLANKS       QQKEY
     C                   MOVEL     *BLANKS       QQPGM
     C                   MOVEL     *BLANKS       QQNAME
     C                   MOVEL     *BLANKS       NMKEY
     C                   MOVEL     *BLANKS       NMNAME
     C                   MOVE      *BLANKS       Q1PGM            10
     C                   MOVE      *BLANKS       Q1KEY            79
     C                   MOVE      *BLANKS       Q1NAME           79
     C*
     C                   MOVEL     CMKEY         QQKEY            79
     C                   MOVEL     CMPNAM        QQPGM            10
     C                   MOVEL     *BLANKS       QQNAME           79
     C                   MOVEL     *BLANKS       CMADJ
     C                   MOVEL     *BLANKS       CMOCST
     C*
     C*
      * *** GET THE KEY/NAME
     C*
     C***
     C     KDKEY         CHAIN     PKD01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        QQPGM
     C     KDKEY         CHAIN     PKD01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   GOTO      ENDGETNAM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* If conversion program name <> blanks; Then call program
     C* to pass back name in requested form
     C*  KDCVTP HOLDS THE CONVERSION PROGRAM NAME (I.E. CR001R...)
     C     KDCVTP        IFNE      *BLANKS
     C                   MOVEL     QQPGM         Q1PGM
     C                   MOVEL     QQKEY         Q1KEY
      *
     C     KDCVTP        IFNE      'CR001R'
     C                   CALL      KDCVTP
     C                   PARM                    Q1PGM
     C                   PARM                    Q1KEY
     C                   PARM                    Q1NAME
      *
     C                   ELSE
      *
     C                   CALL      'CR001R'
     C                   PARM                    Q1PGM
     C                   PARM                    Q1KEY
     C                   PARM                    Q1NAME
     C                   PARM      *BLANKS       #1DTA1           79
     C                   PARM      *BLANKS       #1DTA2           79
     C                   PARM      *BLANKS       CMADJ
     C                   PARM      *BLANK        CMOCST
      *
     C                   ENDIF
     C                   ENDIF
     **
     C                   MOVE      *BLANKS       NMKEY
     C                   MOVEL     Q1KEY         NMKEY            79
     C                   MOVE      *BLANKS       NMNAME
     C                   MOVEL     Q1NAME        NMNAME           79
     **
     **
     **
     c     endgetnam     endsr
     **=============================================================================================
       //===================================================================
       // BldTeamIdList - Build Team Id Listing
       //===================================================================
     P BldTeamIdList   B
     D BldTeamIdList   PI
     D  managerIn                    10

     D rootFound       s               n
     D hldTeamId       s                   like(TEid)
      /Free

       Clear teamIdList;
       kkMgr=ManagerIn;
       // Read thru all teams that user is manager of
       Chain keySMTEL1 pte01;
       If %Found(SMTEL1);
          DoU %Eof(SMTEL1);
             //
             // Is this a root team id (where that manager can't be found
             // managing a team in a paraent team)
             //
             hldTeamId=TEid;
             rootFound=ChkRootTeamId(ManagerIn:TEid);
             //
             // If A parent folder can't be found with this person as manager then
             // add this team to the list that will be used in the second part.
             //
             If rootFound=*Off;
                pos=%LookUp(hldTeamId:teamIdList);
                If pos=0;
                   pos=%LookUp(0:teamIdList);
                   If pos<>0;
                      teamIdList(pos)=hldTeamId;
                   EndIf;
                EndIf;
             EndIf;
             ReadE keySMTEL1 pte01;
          EndDo;
       EndIf;

      /End-Free
     P BldTeamIdList   E
       //===================================================================
       // ChkRootTeamId - Check to see if this team id is not a root.
       //===================================================================
     P ChkRootTeamId   B
     D ChkRootTeamId   PI              n
     D  managerIn                    10
     D  teamIdIn                      5  0

     D manager         s                   like(TSusr)
     D root            s               n
      /Free

       manager=managerIn;
       kkType='T';
       kkUsr=%EditC(teamIdIn:'X');
       // Read thru all teams that user is manager of
       // Is this a Sub Team?
       Chain keySDTSL3 pts03;
       If %Found(SDTSL3);
          kkTeamId=TStId;
          Chain keySMTEL pte00;
          If %Found(SMTEL);
             //
             // Is the parent team manager match this person, if so indicate that a
             // parent root was found to not add this team to the list
             If TEmgr=manager;
                Return *On;
             //
             // Otherwise check to see if the parent has a parent
             Else;
                Return ChkRootTeamId(manager:TStId);
             EndIf;
          EndIf;
       Else;
          //
          // Made it to the primary team and could not find this person
          // set as manager to any of the parents.
          Return *Off;
       EndIf;

      /End-Free
     P ChkRootTeamId   E

    P*====================================================================
    P* GetClaimRRN - Return Claim RRN
    P*====================================================================
     PGetClaimRRN      b                   export
    P*
     DGetClaimRRN      PI             9  0
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inYr                           3  0 const
     D inClaim                        7  0 const

     DclmRrn           s              9  0 inz(0)

      /Free
        If not %open(WMCML1);
           Open WMCML1;
        Endif;

        Chain (inCo#:inGroup:inYr:inClaim) wcm01;
        If %found;
           clmRrn = cmRRN1;
        Endif;

        If %open(WMCML1);
           Close WMCML1;
        Endif;

        Return clmRrn;
      /End-Free

     PGetClaimRRN      e

       //===================================================================
       // GetDiaryCount - Get the Diary Count for the given user
       //===================================================================
     P GetDiaryCount   B
     D GetDiaryCount   PI             5  0
     D  userIn                       10
     D  dateIn                        7

     D date            s              7  0
     D count           s              5  0
     D user            s             10

      /Free
       user=userIn;
       Monitor;
          date=%Dec(dateIn:7:0);
       On-Error 105;
          date=0;
       EndMon;
       If date=0;
          date=9999999;
       Endif;

       exec sql
       SELECT Count(*)
       INTO :count
       FROM SDCMP
       WHERE CMUSER= :user AND ((CMCDAT= :date AND
        :date<>9999999) Or (CMCDAT <= :date And :date=9999999))
        AND CMSTAT<>'I' AND CMDIFL='Y' AND CMDCDT=0;

       Return count;
      /End-Free

     P GetDiaryCount   E

     P*====================================================================
     P* GetEmpYrRR - Get employer policy year RRN
     P*====================================================================

     PGetEmpYrRR       b                   export

     DGetEmpYrRR       PI             9  0
     D inCo#                               like(ELco#)  const
     D inGroup                             like(ELfnd)  const
     D inEmp#                              like(ELemp#) const
     D inDiv                               like(ELdiv)  const
     D inEnv                          1                 const

     D sysDt7          s                   like(ELpfDt)

     DisQuote          s               n   inz(*Off)
     DstrCommand       s            255
     DstrLibrary       s             10    inz(' ')
     DempYrRrn         s              9  0 inz(0)
     Dstatus           s              5  0
     DerrorMsg         s            100

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

      /Free
       strCommand = 'DLTOVR FILE(WDELL)';
       Callp(e) QCMDEXC(strCommand:%len(strCommand));
       status = %status;
       If %Error;
          errorMsg = Q1extp + Q1exnm + '-' + Q1red;
       Else;
          errorMsg = *blanks;
       Endif;
       RtvEnviLib ('*CURRENT':strLibrary:*Blanks);
       If (strLibrary = RtvQuoteLib('*QTE') And env = ' ') Or
        (strLibrary = RtvQuoteLib('*LIV') And env = 'Q');
          If env = 'Q';
             strCommand = 'OVRDBF FILE(WDELL) TOFILE(' +
              %trim(RtvQuoteLib('*QTE')) + '/WDELLQ)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          Else;
             strCommand = 'OVRDBF FILE(WDELL) TOFILE(' +
              %trim(RtvQuoteLib('*LIV')) + '/WDELLQ)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          Endif;
          isQuote=*On;
       Endif;

       If not %open(WDELL);
          Open WDELL;
       Endif;

       sysDt7 = %dec(%date():*cymd);
       Setgt (inCo#:inGroup:inEmp#:inDiv) wel00;
       Dou ELpfDt <= sysDt7 or %eof;
          Readpe (inCo#:inGroup:inEmp#:inDiv) wel00;
       Enddo;
       If %eof;
          Setgt (inCo#:inGroup:inEmp#:inDiv) wel00;
          Readpe (inCo#:inGroup:inEmp#:inDiv) wel00;
       Endif;

       If not %eof;
          empYrRrn=elRRN;
       Endif;

       If %open(WDELL);
          Close WDELL;
       EndIf;

       Return empYrRRN;

      /End-Free

     PGetEmpYrRR       e

     P*====================================================================
     P* GetSisRRN - Get SIS file RRN
     P*====================================================================
     PGetSisRRN        b
     DGetSisRRN        pi             9  0
     D key                           40    const
     D pgm                           10    const

     DSisRRNPgm        pr                  extpgm(sisPgm)
     D inKey                         40    const
     D inPgm                         10    const
     D outRRN                         9  0

     DsisPgm           s             10    inz('CIDARTS1R')
     Drrn              s              9  0

      /Free
       Monitor;
          Callp SisRRNPgm(key:pgm:rrn);
          Return rrn;
       On-error;
          Return *zeros;
       Endmon;
      /End-free
     PGetSisRRN        e

     P*====================================================================
     P* RtvEnviLib - Retrieve the library for the Environment
     P*====================================================================
     PRtvEnviLib       b                   export
     DRtvEnviLib       pi
     D inEnv                         10    const
     D outLib                        10    const
     D outMLib                       10    const

     DRtvEnvLib        pr                  ExtPgm('RTVENVLIB')
     D @@env                         10    const
     D ##lib                         10    const
     D ##Mlib                        10    const

      /Free

       Callp RtvEnvLib(inEnv:outLib:outMLib);
       return;

      /End-Free

     PRtvEnviLib       e

     P*====================================================================
     P* RtvQuoteLib - Retrieve the library for employer
     P*====================================================================

     PRtvQuoteLib      b                   export

     DRtvQuoteLib      pi            10
     D inEnv                          4    const

     DRtvQteLib        pr                  ExtPgm('RTVQTELIB')
     D @@env                          4    const
     D @@lib                         10    const

     DoutLibrary       s             10

      /Free

       Callp RtvQteLib(inEnv:outLibrary);
       Return outLibrary;

      /End-Free
     PRtvQuoteLib      e

       //===================================================================
       // WriteTeamUser - Write to Team User Result Set
       //===================================================================
     P WriteTeamUser   B
     D WriteTeamUser   PI
     D   teamIdIn                          like(TEid)
     D   teamNameIn                        like(TEname)
     D   parentTeamIn                      like(TEname)

     D teamName        s                   like(TEname)
     D hldTeamId       s                   like(TEid)
     D hldType         s                   like(TStype)
     D hldUsr          s                   like(TSusr)
      /Free

       // Read thru all the users/teams associated with this team
       teamName=teamNameIn;
       kkTeamId=teamIdIn;
       Chain key1SDTSL2 pts02;
       If %Found(SDTSL2);
          DoU %Eof(SDTSL2);
             //
             // If this is a user record then write an entry to the teamListing array
             If TStype=*Blanks;
               If TSstat = 'A';
                i+=1;
                teamListing(i).team=teamNameIn;
                teamListing(i).parentTeam=parentTeamIn;
                teamListing(i).teamUser=TSusr;
                teamListing(i).diaryCount=getDiaryCount(TSusr:inCd);
               Endif;
             //
             // If this is a sub-team record then read thru it's values
             Else;
               If TSstat = 'A';
                kkTeamId=%Dec(%Subst(TSusr:1:5):5:0);
                Chain keySMTEL pte00;
                If %Found(SMTEL);
                   //
                   // Save key values since they will be changed in this call
                   hldTeamId=TStId;
                   hldType=TStype;
                   hldUsr=TSusr;
                   //
                   // Process the sub-team entries
                   WriteTeamUser(TEid:TEname:teamName);
                   //
                   // Restore key fields and reset position in file
                   kkTeamId=hldTeamId;
                   kkType=hldType;
                   kkUsr=hldUsr;
                   SetGt key2SDTSL2 pts02;
                EndIf;
              Endif;
             EndIf;
             ReadE key1SDTSL2 pts02;
          EndDo;
       EndIf;

      /End-Free
     P WriteTeamUser   E
