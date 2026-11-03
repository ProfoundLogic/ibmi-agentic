      // @MBRTEXT: Notify user a wider screen size is needed
      *================================================================
      *
      *  Program........: DSPSIZ
      *  Written by.....: M. Pryor
      *  Written for....: Variety Wholesalers, Inc.
      *  Date written...: April 2005
      *  Compiler.......: RPG/400
      *
      *  Purpose........: Notify user a larger screen size is needed.
      *
      *  10/06/05: Renamed DSPSIZ from LADSCN and moved source to
      *            QGPL.
      *
      *================================================================

      * codermake: compile options from the PRFDEMOLIB README (CRTBNDRPG
      * DFTACTGRP(*NO) ACTGRP(*CALLER)) carried in the source.
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     FDspSiz    CF   E             WORKSTN

     C     *ENTRY        PLIST
     C                   PARM                    ProgNm           10

      /FREE
       EXFMT TooBig;
       *INLR = *ON;
      /END-FREE

      /FREE
       BEGSR *InzSr;
          IF %PARMS = 0;
              PgmNam = *BLANKS;
          ELSE;
              PgmNam = ProgNm;
          ENDIF;
       ENDSR;