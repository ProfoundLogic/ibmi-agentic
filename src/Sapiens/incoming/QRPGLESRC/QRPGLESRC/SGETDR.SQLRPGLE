     H*****************************************************************
     H* (C) Copyright FISERV INSURANCE SOLUTIONS, INC. , 2005         *
     H*TITLE SGETDR  - STORED PROCEDURE - GET Dart Records for Grid   *
     H* DESCRIPTION:                                                  *
     F*      Called from Vdarts - to load vdart grid
     ***========================================================================***
     **** Create Procdure must occur on the clients machine as this is installed****
     **     Below is the SQL used on client 129's Machine to "register" the stored proc.
      *
      *   CREATE PROCEDURE
      *   pdsobj/SGETDR(
      *   IN INKY    CHar(40))
      *
      *   RESULT SETS 1
      *
      *   LANGUAGE RPGLE
      *   EXTERNAL NAME
      *   pdsobj/SGETDR
      *   READS SQL DATA
      *   PARAMETER STYLE SQL
     ***========================================================================***
     H*****************************************************************
     H*  CREATED: 12/19/05                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   60    GENERAL CHAIN                                         *
     H*****************************************************************
     FSDCMLA    IF   E           K DISK
     FSMARL     IF   E           K DISK
     FSTSML     IF   E           K DISK
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
     c*
     *** Get Summary Code description for output to SCDDESC
     c     sck           chain     psm01                              51
     C     *in51         ifeq      *off
     c*** hit on SM file.. set indicator in multioccur ds to pass back
     C                   eval      scddesc=smdesc
     C                   else
     C                   eval      scddesc=*blanks
     C                   endif
     c*
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
