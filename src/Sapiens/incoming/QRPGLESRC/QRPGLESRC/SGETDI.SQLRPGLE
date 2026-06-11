     H*****************************************************************
     H* (C) Copyright FISERV INSURANCE SOLUTIONS, INC. , 2005         *
     H*TITLE SGETDI  - STORED PROCEDURE - GET Dart Records for Grid   *
     H* DESCRIPTION:                                                  *
     F*      Called from Vdarts - to load Diary grid
     ***========================================================================***
     **** Create Procdure must occur on the clients machine as this is installed****
     **     Below is the SQL used on client 129's Machine to "register" the stored proc.
      *
      *   CREATE PROCEDURE
      *   pdsobj/SGETDi(
      *   IN INUSEER CHar(10),
      *      INCDAT  CHAR(7))
      *
      *   RESULT SETS 1
      *
      *   LANGUAGE RPGLE
      *   EXTERNAL NAME
      *   pdsobj/SGETDi
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
     FIAKNL1    IF   E           K DISK
     FSMARL     IF   E           K DISK
     FSTSML     IF   E           K DISK
     FSMKDL     IF   E           K DISK
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     DSDCMP          E DS                  EXTNAME(SDCMPDS) OCCURS(9999)
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
     I*
      *
     C** DART FILE
     C     CMK           KLIST
     C                   KFLD                    INUSER
     C                   KFLD                    INCDAT
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
     *******
     C*   KEY LIST FOR THE KEY DEFINITION FILE  *** ADDED TO GET NAME ****
     C*
     C     KDKEY         KLIST
     C                   KFLD                    QQPGM
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
      *
     C                   MOVE      INCD          INCDAT            7 0
     C*
     **** SET INDEX TO 0
     C                   EVAL      I=0
     C*
     C*** USE KEY TO "GET" ALL RECORDS THAT MEET CRITERIA
     C*** Read DARTS Remark file
     C     CMK           SETGT     PCM01
     C     CMK1          readpe    PCM01                                  50    User only get
     C     *in50         ifeq      *off                                          if hit
     C     CMCDAT        andle     INCDAT                                        and rcd date is le
     C** hit

     C*** INCREMENT INDEX
     C                   EVAL      I=I+1
     C*** SET OCCURANCE FOR DS
     C     I             OCCUR     SDCMP
     c                   else
     c                   goto      norcd
     c                   endif
      ****
     C***  CMK           READE     PCM01                                  50
     C     *IN50         DOWEQ     *OFF
     C     CMCDAT        andle     INCDAT
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
     c****************** goto      notnow  ***** ADDED BACK IN THE GETNAMES ********
     C                   EXSR      GETNAM
     c***  notnow        tag  ******************************************************
     ***
     c*
     c                   eval      i=I+1
     C     I             OCCUR     SDCMP
     C*
     C*** Read next DARTS Remark Record
     C     CMK1          readpe    PCM01                                  50
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
     C                   CALL      KDCVTP
     C                   PARM                    Q1PGM
     C                   PARM                    Q1KEY
     C                   PARM                    Q1NAME
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
