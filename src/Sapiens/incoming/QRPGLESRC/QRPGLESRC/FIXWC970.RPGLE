      *****************************************************************
      * (C) Copyright Progressive Data Solutions, Inc., 1996 - 1999
      *****************************************************************
      * Fix program for claims only clients to populate rate set code *
      *                                                               *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *****************************************************************
      *
     * Employer Master File
     FWmeml     IF   E           K Disk
      *
     * Employer Detail File
     FWdelp     UF   E             Disk
     *
     * Rate Set Code Master File
     FWmrsl     UF A E           K Disk
     *
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
     D Timestmp        S             12  0
     D Today           S               D   DATFMT(*USA)
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *
     C                   Eval      *Inlr = *Off
      /eject
      *
     C     Wmeml_Key     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
      *
     C     Wmrsl_Key     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELRTCD
      *
     C                   TIME                    DSDTTM
     C                   Move      Udate         Today
      *
      /eject
      **********************************************************************
      *   M A I N L I N E   R O U T I N E                                  *
      **********************************************************************
      *
      *  Loop Through Wdelp
      *
     C                   Dou       %Eof(Wdelp)
     C                   Read      Wdelp
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        Elrtcd <> *Blank
     C                   Iter
     C                   Endif
      *
     C     Wmeml_Key     Chain     Wmeml
      *
     C                   If        %Found
      *
     C                   Select
     C                   When      Empst <> *Blank
     C                   Movel     Empst         Elrtcd
     C                   When      Emmst <> *Blank
     C                   Movel     Emmst         Elrtcd
     C                   Other
     C                   Move      *Blanks       Elrtcd
     C                   Endsl
      *
     C                   Update    Pel01
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        Not %Found
   37C                   Eval      RSCO#  = ELCO#                                 COMPANY NUMBER
   37C                   Eval      RSRTCD = ELRTCD                                RATE SET CODE
   37C                   Eval      RSDESC = ELRTCD                                DESCRIPTION
   37C                   Eval      RSCRFL = 'Y'                                   CONSENT TO RATE
   38C                   Eval      RSSTAT = 'A'                                   STATUS
   38C                   Eval      RSUSER = Q1USER                                USER
   38C                   Eval      RSPGM  = Q1PGM                                 PROGRAM
   38C                   Eval      RSTIME = DSTIME                                TIME
   38C     *Mdy          Move      Today         Rsdt                             DATE
   38C                   Eval      RSIPGM = Q1PGM                                 INITIAL PROGRAM
   38C                   Eval      RSIUSR = Q1USER                                RECORD CREATED BY
   38C     *Mdy          Move      Today         Rsidt                            DATE
   38C                   Eval      RSJRST = ELRTCD                                PREMIUM RATING STA
     C                   Write     Prs01
     C                   Endif
     C                   Endif
      *
     C                   Enddo
      *
     C                   Eval      *Inlr = *On
      *
