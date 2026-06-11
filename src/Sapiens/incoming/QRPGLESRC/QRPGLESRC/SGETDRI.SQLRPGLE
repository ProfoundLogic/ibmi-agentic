     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
      *
     h option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     h debug(*yes)
      *
     H*TITLE SGETDR  - STORED PROCEDURE - GET Dart Records for Grid   *
     H* DESCRIPTION:                                                  *
     H*      Called from idarts - to load vdart grid
      *****************************************************************
     H* REVISIONS
      *
      * 08/29/23  C:062  Ticket 113718(150)
      *                  Expand the result set.
      *
      * 06/26/14  C:300  Project DR312
      *                  DARTMAIL- Return Emailed flag
      *
      * 06/09/11  C:061  Project 133IKU
      *                  Populate 'Claim Adjuster' and 'Claim Status'
      *                  fields.  Change call to CR001R to get new field
      *                  values.
      *
     H*
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
     H*   09/03/09 C:232 SUPPORT: 69600
     H*                  idarts - Additional Remarks Field issue.
     H*
     H*   12/05/06 - India change - Added SMMD for this proc.
     H*
     ***========================================================================***
     **** Create Procdure must occur on the clients machine as this is installed****
     **     Below is the SQL used on client 129's Machine to "register" the stored proc.
      *
      *   CREATE PROCEDURE
      *   pdsobj/SGETDRi(
      *   IN INKY    CHar(40))
      *
      *   RESULT SETS 1
      *
      *   LANGUAGE RPGLE
      *   EXTERNAL NAME
      *   pdsobj/SGETDRi
      *   READS SQL DATA
      *   PARAMETER STYLE SQL
     ***========================================================================***
     H*****************************************************************
     H*  CREATED: 12/19/05                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H* 10/06/09 C:220 SUPPORT: 70088                                 *
     H*                Increase in len of fields NUMATTACH to 5,0.    *
     H*                -Just compiled.                                *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   60    GENERAL CHAIN                                         *
     H*****************************************************************
     FSDCMLA    IF   E           K DISK
     FSMARL     IF   E           K DISK
     FSTSML     IF   E           K DISK
      ******* beg add ****** 06/09/11 *********************************
     FSMKDL     IF   E           K DISK
      ******* end add ****** 06/09/11 *********************************
     FSMMDL     IF   E           K DISK


     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** Begin Chg *** 08/29/23 *******************************************
     D***SDCMP          E DS                  EXTNAME(SDCMPDS1) OCCURS(9999)
     DSDCMP          E DS                  EXTNAME(SDCMPDS1) OCCURS(15500)
     D*** End   Chg *** 08/29/23 *******************************************
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
      ***** BEGIN ADD *****  09/03/09 *********************************
     D@count           s              5  0 inz
      ***** END   ADD *****  09/03/09 *********************************
     I*
      ******* beg add ****** 06/09/11 *********************************
     C*
     C*   KEY DEFINITION FILE  *** ADDED TO GET NAME ****
     C*
     C     KDKEY         KLIST
     C                   KFLD                    KKPGM            10
      ******* end add ****** 06/09/11 *********************************
      *
     C** DART FILE
     C     CMK           KLIST
     C                   KFLD                    INKY
      *
     C*  Add rmk file
     C     ark           KLIST
     C                   KFLD                    INKY
     C                   KFLD                    CMSEQ#
      *
     C*  Summ code file
     C     sck           KLIST
     C                   KFLD                    CMSMCD
     C*  Add mdk file
     C     mdk           KLIST
     C                   KFLD                    INKY
     C                   KFLD                    CMSEQ#
      *
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
     C                   PARM                    INKY             40
      *
     C*
     **** SET INDEX TO 0
     C                   EVAL      I=0
     C*
     C*** USE KEY TO "GET" ALL RECORDS THAT MEET CRITERIA
     C*** Read DARTS Remark file
     C     CMK           SETLL     PCM01                                  50
     C     *in50         ifeq      *on
     C** hit

     C*** INCREMENT INDEX
     C                   EVAL      I=I+1
     C*** SET OCCURANCE FOR DS
     C     I             OCCUR     SDCMP
     c                   else
     c                   goto      norcd
     c                   endif
      ****
     C     CMK           READE     PCM01                                  50
     C     *IN50         DOWEQ     *OFF
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
     c*** Begin Add *** 10/05/10 *******************************************
     c                   if        adlrmkf='N'
     c*** End   Add *** 10/05/10 *******************************************
     c*
      ***** BEGIN ADD *****  09/03/09 *********************************
      ***  Begin Del  ***  02/18/11  **************************************
     C**********/EXEC SQL
     C*****+ SELECT count(*) INTO :@count FROM smclp WHERE clkey = :inky and
     C*****+ clseq = :cmseq#
     C**********/End-EXEC
      ***  End   Del  ***  02/18/11  **************************************
     c*** Begin Del *** 10/05/10 *******************************************
     c**********         if        adlrmkf='N'
     c*** End   Del *** 10/05/10 *******************************************
      *
     c                   if        @count <> 0
     c                   eval      adlrmkf='Y'
     c                   else
     c                   eval      adlrmkf='N'
     c                   endif
      *
     c                   endif
      ***** END   ADD *****  09/03/09 *********************************
     *** Get Summary Code description for output to SCDDESC
     c     sck           chain     psm01                              51
     C     *in51         ifeq      *off
     c*** hit on SM file.. set indicator in multioccur ds to pass back
     C                   eval      scddesc=smdesc
     C                   else
     C                   eval      scddesc=*blanks
     C                   endif
     c*
     *** Get Number of attachment in MDK file
     c                   Z-add     *zeros        NumAttach
     c     mdk           chain     pmd01                              51
     C     *in51         doweq     *off
     c                   Eval      NumAttach = NumAttach + 1
     c     mdk           reade     pmd01                                  51
     c                   enddo
      *
      ******* beg add ****** 06/09/11 *********************************
     C                   EXSR      GETNAM
      ******* end add ****** 06/09/11 *********************************
      *** Beg Add *** 06/26/14 *********************************************************************
      * Assign Emailed flag from SDCMP.CMEMAILFLG
      /free
          EMailed = CMemailflg;
      /end-free
      *** End Add *** 06/26/14 *********************************************************************
     c                   eval      i=I+1
     C     I             OCCUR     SDCMP
     C*
     C*** Read next DARTS Remark Record
     C     CMK           READE     PCM01                                  50
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
     C*
     C*** END PROGRAM
     C*
     c     endit         tag
     C                   MOVE      *ON           *INLR
     C*
      ******* beg add ****** 06/09/11 *********************************
     C*
     **=============================================================================================
     c     GETNAM        begsr
     **=============================================================================================
     **
     C*** INITIALIZE THE PARMS FOR PROGRAM CALL
     C*
     C                   MOVEL     *BLANKS       NMKEY
     C                   MOVEL     *BLANKS       NMNAME
     C*
     C                   MOVEL(P)  CMKEY         #1KEY            79
     C                   MOVEL(P)  CMPNAM        #1PGM            10
     C                   MOVE      *BLANKS       #1NAME           79
     C                   MOVEL     *BLANKS       CMADJ
     C                   MOVEL     *BLANKS       CMOCST
      *
      * *** GET THE KEY/NAME
      *
     C                   MOVEL(P)  CMPNAM        KKPGM
     C     KDKEY         CHAIN     PKD01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       KKPGM
     C     KDKEY         CHAIN     PKD01                              60
     C   60              LEAVESR
     C                   ENDIF
     C*
     C* If conversion program name <> blanks; Then call program
     C* to pass back name in requested form
     C*  KDCVTP HOLDS THE CONVERSION PROGRAM NAME (I.E. CR001R...)
     C*
     C     KDCVTP        IFNE      *BLANKS
      *
     C     KDCVTP        IFNE      'CR001R'
      *
     C                   CALL      KDCVTP
     C                   PARM                    #1PGM
     C                   PARM                    #1KEY
     C                   PARM                    #1NAME
      *
     C                   ELSE
      *
     C                   CALL      'CR001R'
     C                   PARM                    #1PGM
     C                   PARM                    #1KEY
     C                   PARM                    #1NAME
     C                   PARM      *BLANKS       #1DTA1           79
     C                   PARM      *BLANKS       #1DTA2           79
     C                   PARM      *BLANKS       CMADJ
     C                   PARM      *BLANK        CMOCST
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL(P)  #1KEY         NMKEY            79
     C                   MOVEL(P)  #1NAME        NMNAME           79
      *
     c     endgetnam     endsr

      ******* end add ****** 06/09/11 *********************************
