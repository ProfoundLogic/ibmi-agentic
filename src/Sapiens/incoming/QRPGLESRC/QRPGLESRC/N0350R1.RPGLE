     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE N0350R - WORK WITH EOMB'S INVOICES
     H*****************************************************************
     H*  Created: 03/08/99  C:56 Project: MC152                       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/16/99  C:53  MC145
     H*                  CONDITION CALL TO WCMS PGMS TO BE BASED ON   *
     H*                  PDS WORKERS COMP BEING INSTALLED.            *
     H*****************************************************************
     H*

     D*
     D                 DS
     D  XXA101                 1     10
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN1            9 0          FAEOP
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
     C*
     C* LIKE DEFINES
     C*
     C*** BEG ADD *** 06/16/99 *****************************
     C*
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C*** END ADD *** 06/16/99 *****************************
     C*
     C** CALL WORK WITH INVOICES PROGRAM
     C*
     C                   Z-ADD     @@RRN1        ##RRN1
     C     INVC          TAG
     C                   CALL      'N0350R'
     C                   PARM      '1'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN1            9 0          faeop
     C*
     C     ##KC          IFEQ      '1'
     C                   MOVE      '1'           @@KC
     C                   GOTO      EOJ
     C                   ENDIF                                                  ##KC = 1
     C     ##KL          IFEQ      '1'
     C                   MOVE      '1'           @@KL                           ##KL = 1
     C                   GOTO      EOJ
     C                   ENDIF
     C*
     C* CALL WORK WITH CHECKS AND CHECK REQUESTS IF CASE IS LINKED
     C* WITH WORKER'S COMP
     C*
     C                   Z-ADD     @@CO#         ##CO#             3 0
     C                   Z-ADD     @@SUB#        ##SUB#            5 0
     C                   Z-ADD     @@FND         ##FND             3 0
     C                   MOVEL     @@CO#         #1CO#
     C                   MOVEL     @@SUB#        #1SUB#
     C                   MOVEL     @@FND         #1FND
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM      ' '           ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'N'
     C*
     C* WORK WITH CHECK REQUESTS
     C*
     C                   Z-ADD     @@FYR         ##FYR             3 0
     C                   Z-ADD     @@CASE        ##CASE            7 0
     C                   Z-ADD     @@EOB#        ##EOB#            9 0
     C     REQST         TAG
     C*** BEG ADD *** 06/16/99 *****************************
     C     ##WCIF        IFEQ      'Y'
     C     ##NPWC        ANDNE     'Y'
     C*** END ADD *** 06/16/99 *****************************
     C                   CALL      'W0584R'
     C                   PARM      '1'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN1                         faeop
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##EOB#
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C                   GOTO      INVC
     C                   ENDIF                                                  ##KC = 1
     C*
     C* WORK WITH CHECKS
     C*
     C                   CALL      'W0584R1'
     C                   PARM      '1'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN1                         faeop
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##EOB#
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C                   GOTO      REQST
     C                   ENDIF                                                  ##KC = 1
     C                   ENDIF                                                  ##WCIF
     C*
     C                   ENDIF                                                  ##OUTS = N
     C*
     C     EOJ           TAG
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
