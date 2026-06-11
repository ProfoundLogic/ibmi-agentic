     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* API - WA036R1 - UPDATE ALL WCMS EMPLOYER FILES                      A
     H*****************************************************************
     H*                                                               *
     H*    CREATED: 10/17/03  C: 53 Proj: 126WBP                      *
     H*****************************************************************
     H*                                                               *
     H* This API is part of the the Tranfer process of moving         *
     H* an employer from one group to another.  Specifically this     *
     H* program updates all the files with the changed group and      *
     H* employer information. This is the 2nd of two APIs.  This one  *
     H* completes the update task for affected files because the total*
     H* exceeds 50; the compiler limit for processing these files.
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/10/10  C:33  Project: 139WPL                               *
     H*                 Include HCN Program information               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*  60         GENERAL USAGE (CHAIN)                             *
     H*                                                               *
     H*****************************************************************
      *
      * Worker's Comp User Destination Master File
     FWMUDL2    UF   E           K DISK
      *
      * Loss Control Task File
     FWMVSL     UF   E           K DISK
      *
      * Waiver Information File
     FWMWVL1    UF   E           K DISK
      * GL Interface Transaction File
     FWAGLL2    UF   E           K DISK
     F***  Begin Add  ***  11/10/10  **************************************
      * HCN Program Info File
     FWDHCL3    UF   E           K DISK
     F                                     RENAME(PHC01:PHC03)
     FWDHCLR    UF   E           K DISK    USROPN
     F                                     RENAME(PHC01:PHC0R)

     D*
     D OVRHCR          C                   CONST('OVRDBF FILE(WDHCLR) -
     D                                     MBR(REVISED) -
     D                                     OVRSCOPE(*JOB)')
     F***  End   Add  ***  11/10/10  **************************************
      *
      *
      * Worker's Comp User Destination Master File

     IPUD01
     I              UDCO#                       XXCO#
     I              UDFND                       XXFND
     I              UDEMP#                      XXEMP#
     I              UDDIV                       XXDIV
      * Loss Control Task File
     IPVS01
     I              VSCO#                       XXCO#
     I              VSFND                       XXFND
     I              VSEMP#                      XXEMP#
     I              VSDIV                       XXDIV
      * Waiver Information File
     IPWV01
     I              WVCO#                       XXCO#
     I              WVFND                       XXFND
     I              WVEMP#                      XXEMP#
     I              WVDIV                       XXDIV
      * GL Interface Transaction File
     IPGL01
     I              GLCO#                       XXCO#
     I              GLFND                       XXFND
     I              GLEMP#                      XXEMP#
     I              GLDIV                       XXDIV
     I***  Begin Add  ***  11/10/10  **************************************
      * HCN Program Info File
     IPHC03
     I              HCCO#                       XXCO#
     I              HCFND                       XXFND
     I              HCEMP#                      XXEMP#
     I              HCDIV                       XXDIV
     IPHC0R
     I              HCCO#                       XXCO#
     I              HCFND                       XXFND
     I              HCEMP#                      XXEMP#
     I              HCDIV                       XXDIV
     I***  End   Add  ***  11/10/10  **************************************
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FGRP            3
     C                   PARM                    @@FEMP            9
     C                   PARM                    @@FDIV            5
     C                   PARM                    @@TGRP            3
     C                   PARM                    @@TEMP            9
     C                   PARM                    @@TDIV            5
      *
      *
      *****************************************************************
      *  KEYS
      *****************************************************************
      *
      * Key used to retrieve 'From' data from files
      *
     C     FRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFFND
     C                   KFLD                    XXFEMP
     C                   KFLD                    XXFDIV
      *
      *****************************************************************
      *  LIKE DEFINITIONS
      *****************************************************************
      *
     C     *LIKE         DEFINE    XXFND         XXFFND
     C     *LIKE         DEFINE    XXEMP#        XXFEMP
     C     *LIKE         DEFINE    XXDIV         XXFDIV
      *****************************************************************
      *  MAIN
      *****************************************************************
      *
      * initialize key variables
      *
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FGRP        XXFFND
     C                   MOVE      @@FEMP        XXFEMP
     C                   MOVE      @@FDIV        XXFDIV
      *
     C*
     C     FRKY          CHAIN     PUD01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PUD01
     C     FRKY          READE     PUD01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PVS01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PVS01
     C     FRKY          READE     PVS01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PWV01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PWV01
     C     FRKY          READE     PWV01                                  60
     C                   ENDDO
     C*
     C     FRKY          CHAIN     PGL01                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PGL01
     C     FRKY          READE     PGL01                                  60
     C                   ENDDO
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C     FRKY          CHAIN     PHC03                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PHC03
     C     FRKY          READE     PHC03                                  60
     C                   ENDDO
     C*
     C                   CALL      'QCAEXEC'
     C                   PARM      OVRHCR        QCMD             55
     C                   PARM      55            LENGTH           15 5
     C                   OPEN      WDHCLR
     C     FRKY          CHAIN     PHC0R                              60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      LOADTO
     C                   UPDATE    PHC0R
     C     FRKY          READE     PHC0R                                  60
     C                   ENDDO
     C                   CLOSE     WDHCLR
     C***  End   Add  ***  11/10/10  **************************************
      *
     C                   RETURN
      *
      *****************************************************************
      *  LOADTO
      *****************************************************************
      *
     C     LOADTO        BEGSR
      *
     C                   MOVE      @@TGRP        XXFND
     C                   MOVE      @@TEMP        XXEMP#
     C                   MOVE      @@TDIV        XXDIV
      *
     C                   ENDSR
