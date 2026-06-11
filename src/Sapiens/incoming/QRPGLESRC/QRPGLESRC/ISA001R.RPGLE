     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     ISA001R                                       **
     H* DESCRIPTION: EXIT PROGRAM FOR VIEW FROM WRK W/ QUEUES      **
     H*              CALL IMAGE NOTES APPLICATION                  **
     H* CREATED BY:
     H*
     H*** REVISIONS:
     H*
     H***************************************************************
      *

     C     *ENTRY        PLIST
     C                   PARM                    ##QID            10            10A Q ID
     C                   PARM                    ##USER           10            10A USER
     C                   PARM                    ##SDT             8 0           80 SCNDTMDY
     C                   PARM                    ##STM             6 0           60 SCTIM
     C                   PARM                    ##DCID           12            12A DCID
     C                   PARM                    ##DESC           30            30A DC DSC
     C                   PARM                    ##DOCT            8             8A DOCTYP
     C                   PARM                    ##FL01            1             1A IMG/FAX
     C                   PARM                    ##PTH            63            63A FULL PTH
     C                   PARM                    ##FLR             8             8A FOLD
     C                   PARM                    ##SFLR           12            12A SUBFLR
     C                   PARM                    ##RTNC            1             1A RTNCODE
      *
     C***
     C                   CALL      'IS016R'
     C                   PARM                    ##DCID
     C                   PARM      'ISA001R '    XXPNAM           10
     C*
     C*
     C                   MOVE      *BLANK        ##RTNC
     C     ENDPGM        TAG
     C                   MOVE      *ON           *INLR
