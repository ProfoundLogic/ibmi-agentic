     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *
      /TITLE F2014R - PROVIDER SELECTION
      * *****************************************************************
      *                                                                 *
      *  Description:  THIS PROGRAM WILL DISPLAY A WINDOW TO ALLOW      *
      *                THE USER TO SELECT WHETHER TO GET THE PROVIDER
      *                FROM THE MASTER FILE
      *                OR THE CASE PROVIDER FILE.
      *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                                                                 *
      *  Called From:  U0060R01                                         *
      *                                                                 *
      *  Calls To   :  F0031R - PROVIDER MASTER                         *
      *                F0277R - CASE PROVIDER SEARCH                    *
      *                                                                 *
      *  Created    :   5/08/98  C:47  Project:  98MAG                  *
      * *****************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      *  09/24/01 C:62 PROJECT MC248                                    *
      *                ADD PARM OF CALL OF F0277R.                      *
      *                                                                 *
      *  05/25/99 C:62 PROJECT MC145                                    *
      *                IF CASE MANAGEMENT NOT INSTALLED THEN JUST GO    *
      *                TO F0032R.                                       *
      *                                                                 *
      *  07/10/98 C:61 PROJECT 98MAG                                    *
      *                Change call to provider master F0032R.           *
      *                                                                 *
      *                                                                 *
      *  PLEASE ADD TO TOP                                              *
      * *****************************************************************
      *                                                                 *
      *  Indicators:                                                    *
      *                                                                 *
      *    60 - General File Access                                     *
      *                                                                 *
      * *****************************************************************
      *                                                                 *
      *                                                                 *
     FF2014F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          FAPPP
     C                   PARM                    @@RRN2            9 0          FMCMP
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    @@RTCD            1
      *
      * ---------------------------------------------------------------
     C                   MOVEL(P)  Q1PGM         SCPGM
     C*** BEGIN ADD *** 05/25/99 **************************************
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*** END   ADD *** 05/25/99 **************************************
      *
     C     *INKL         DOUEQ     *ON
      *
     C     @@RRN         IFNE      *ZERO
     C     QQFEIN        ORNE      *BLANK
     C                   LEAVE
     C                   END
      *
     C*** BEGIN ADD *** 05/25/99 **************************************
     C     ##MED         IFEQ      'Y'
     C*
     C                   MOVEL(P)  '1'           SCSEL2
     C*** END   ADD *** 05/25/99 **************************************
     C                   EXFMT     WDW01
     C                   MOVEA     '000'         *IN(71)
     C*** BEGIN ADD *** 05/25/99 **************************************
     C                   ELSE
     C*
     C                   MOVEL(P)  '1'           SCSEL2
     C*
     C                   ENDIF
     C*** END   ADD *** 05/25/99 **************************************
      *
     C     *INKL         IFEQ      *OFF
      *
     C                   SELECT
      *
     C     SCSEL1        WHENEQ    '1'
     C     SCSEL2        ANDEQ     '1'
     C                   MOVE      *ON           *IN71                          SELECTED BOTH
      *
     C     SCSEL1        WHENNE    '1'
     C     SCSEL1        ANDNE     *BLANK
     C                   MOVE      *ON           *IN72                          INVALID CODE
      *
     C     SCSEL2        WHENNE    '1'
     C     SCSEL2        ANDNE     *BLANK
     C                   MOVE      *ON           *IN73                          INVALID CODE
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CASE PROVIDER SEARCH
      *
     C     SCSEL1        WHENEQ    '1'
     C*
     C                   CALL      'F0277R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN                          FAPPP
     C                   PARM                    @@RRN2                         FMCMP
     C*** BEGIN ADD *** 09/24/01 **************************************
     C                   PARM      'Y'           ##MODE            1            FMCMP
     C*** END   ADD *** 09/24/01 **************************************
     C*
     C                   MOVE      *BLANK        SCSEL1
     C*
     C     @@RRN         IFEQ      *ZERO
     C     *INKL         ANDEQ     *OFF
     C                   MOVE      *ON           *INKL
     C                   ENDIF
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * PROVIDER MASTER SEARCH
      *
     C     SCSEL2        WHENEQ    '1'
      ***** BEG DEL ******* 07/10/98 **********************************
     C*************        CALL 'F0031R'
     C*************        PARM           @@CALL
     C*************        PARM           QQFEIN  9
     C*************        PARM           QQMOD   3
     C*************        PARM *BLANK    QQRTCD  1
     C*************        PARM *BLANK    QQTYPE  1
      ***** END DEL ******* 07/10/98 **********************************
      ***** BEG ADD ******* 07/10/98 **********************************
     C                   CALL      'F0032R'
     C                   PARM                    @@CALL            1
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM      *BLANK        QQTYPE            1
     C                   PARM      'Y'           @@1N4             1            type 1 & 4 only
      ***** END ADD ******* 07/10/98 **********************************
     C*
     C                   MOVE      *BLANK        SCSEL2
     C                   MOVE      *ON           *INKL
     C*
     C                   ENDSL
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CLEAR 'SEARCHING' MSG
     C                   CALL      'SRRMVMSG'                           99
      *
     C                   END
      *
     C                   ENDDO
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C     @@RRN         IFEQ      *ZERO
     C     QQFEIN        ANDEQ     *BLANK
     C                   MOVE      '1'           @@RTCD
     C                   END
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
