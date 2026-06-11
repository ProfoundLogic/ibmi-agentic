     H*====================================================================
     H* Revisions:
     H*
     H* 08/31/16  C:33  Project: Elevate
     H*                 Call the program again to close file(s)
     H*====================================================================

     D                 DS                  INZ
     D POMFDS                       475
     D  CLID                         12    OVERLAY(POMFDS:1)
     D  CONR                          2    OVERLAY(POMFDS:13)
     D  PRFX                          4    OVERLAY(POMFDS:15)
     D  PNUM                          9P 0 OVERLAY(POMFDS:19)
     D  GEN#                          3    OVERLAY(POMFDS:154)
     D  SUB#                          6    OVERLAY(POMFDS:157)
     D  PRD#                          9    OVERLAY(POMFDS:163)
     ******
     D DSCLIENT        S             12A   INZ
     D CMCLIENT        S             12A   INZ
     D SUBSET          S              1A   INZ(*OFF)
     D PARM0256        S            256A   INZ
     D PARM1024        S           1024A   INZ
     D XXRTAID         S              7A   INZ
     ****************
     D DATE_ISO        S               D   INZ DATFMT(*ISO)
     ****************
      * Accounting date from IBCHKM
     D                 DS                  INZ
     D DSACCTDTE                       D   DATFMT(*ISO)
     D   EFFCN                        2  0 OVERLAY(DSACCTDTE:1)
     D   EFFYY                        2  0 OVERLAY(DSACCTDTE:3)
     D   EFFMM                        2  0 OVERLAY(DSACCTDTE:6)
     D   EFFDD                        2  0 OVERLAY(DSACCTDTE:9)
     ****
     D PRNTDS        E DS                  EXTNAME(CIPRNTDS)
     C     *ENTRY        PLIST
     C                   Parm                    QQCOI             2
     C                   PARM                    QQPREI            4
     C                   PARM                    QQPOLI            9              chg to Numeric
     C                   PARM                    QQINSN           79
     **
     **
     C     *MDY          MOVE      UDATE         DATE_ISO
     C                   EXSR      CIPOMF
     C                   EXSR      RTVINS
     C*
     C                   eval      *INLR = *ON
     ***
      *---------------------------------------------------------------*
      *     R T V I N S  -  Retrieve named insured name               *
      *---------------------------------------------------------------*
     C     RTVINS        BEGSR

     C                   CLEAR                   PRNTDS
     C                   MOVEL     CMCLIENT      PTCLID                           From Policy rcd?
     C                   MOVEL     QQCOi         PTCONR
     C                   MOVEL     QQPREI        PTPRFX
     C                   Z-ADD     PNUM          PTPLNR
     C                   MOVEL     *BLANKS       PTPLLC
     C                   EVAL      DSACCTDTE =   DATE_ISO
     C                   Z-ADD     EFFCN         PTEFCN
     C                   Z-ADD     EFFYY         PTEFYY
     C                   Z-ADD     EFFMM         PTEFMM
     C                   Z-ADD     EFFDD         PTEFDD
     C                   Z-ADD     EFFCN         PTMLCN
     C                   Z-ADD     EFFYY         PTMLYY
     C                   Z-ADD     EFFMM         PTMLMM
     C                   Z-ADD     EFFDD         PTMLDD
     C                   MOVE      'Y'           PTUPCS

      * Retrieve insured name
     C                   MOVEL     'INS'         PTRQTP
     C                   CALL      'CI00771R'                           99
     C                   PARM                    PRNTDS

     C                   EVAL      QQINSN   = PTNAME

     C***  Begin Add  ***  08/31/16  **************************************
      * Close file(s)
     C                   CLEAR                   PRNTDS
     C                   MOVEL     'CLS'         PTRQTP
     C                   CALL      'CI00771R'                           99
     C                   PARM                    PRNTDS
     C***  End   Add  ***  08/31/16  **************************************

     C                   ENDSR
      *---------------------------------------------------------------*
      *  C I P O M F  -   Retrieve policy master record               *
      *---------------------------------------------------------------*
     C     CIPOMF        BEGSR
      *
     C                   CLEAR                   POMFDS
     C                   EVAL      CONR = QQCOI
     C                   EVAL      PRFX = QQPREI
     C                   MOVE      QQPOLI        PNUM

     C                   CALL      'CI00019R'                           99
     C                   PARM      'CIPOMF08'    FILEPARM         10
     C                   PARM      'I'           SUBSET
     C                   PARM      *BLANKS       XXRTAID
     C                   PARM                    POMFDS

     C     XXRTAID       IFEQ      *BLANKS
     C                   EVAL      DSCLIENT  = CLID
     C                   EVAL      CMCLIENT  = CLID
     C                   ELSE
      ***  If there is a problem with finding the policy master record,
      ***     blank out the agent & client ID to avoid corrupt data.
     C                   EVAL      DSCLIENT  = *BLANKS
     C                   EVAL      CMCLIENT  = *BLANKS
     C                   ENDIF
      *
     C                   ENDSR
