     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTWTCRP - CONVERT CANCELLATION REASON CODE RECORDS
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     This program reads the rate set code file.  For every     *
     H*     unique state in the file it does the following:           *
     H*       It finds a record in the canx/rein state defn file with *
     H*       a corresponding state.  It then finds the most recent   *
     H*       effective date for that co#/fnd/st.  It then gets the   *
     H*       canx notice days, addtnl mailing days, and reinstatement*
     H*       fee values.                                             *
     H*         The program then reads the canx reason code file. For *
     H*         every record with a blank state, it writes a new      *
     H*         record, with an eff date of 0500101 and filling in    *
     H*         state, days and fee from above.                       *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:87                                            *
     H*   CREATED:    11/20/98    PROJECT: WC852                      *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/04/98  C:35   PROJECT: SUPPORT                             *
     H*                  PROGRAM WAS GETTING INTO AN INFINITE LOOP IF *
     H*                  IT ENCOUNTERED A RATE SET CODE RECORD WITH A *
     H*                  BLANK JURISDICTION STATE.  CHANGED CODE TO   *
     H*                  SKIP THESE RECORDS.                          *
     H*                                                               *
     H* 11/30/98  C:35   PROJECT: SUPPORT                             *
     H*                  ADDED CODE TO DELETE OLD TABLE FILE RECORDS. *
     H*                                                               *
     H*****************************************************************
     FWMRSL1    IF   E           K DISK
     FWTXCL     IF   E           K DISK
     FWTCRL     UF A E           K DISK

     C*
     C     RSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKJRST
     C*
     C     XCKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C     XCKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKJRST
     C*
     C     CRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C*
     C     *LIKE         DEFINE    RSCO#         KKCO#
     C     *LIKE         DEFINE    XCFND         KKFND
     C     *LIKE         DEFINE    RSJRST        KKJRST
     C     *LIKE         DEFINE    CRST          KKST
     C     *LIKE         DEFINE    XCNOTC        XXNOTC
     C     *LIKE         DEFINE    XCADDM        XXADDM
     C     *LIKE         DEFINE    XCRFEE        XXRFEE
     C*
     C*  MAIN PROCESSING
     C*
     C                   Z-ADD     *LOVAL        KKCO#
     C                   MOVE      *LOVAL        KKJRST
     C                   MOVE      *BLANKS       KKST
     C                   MOVE      *BLANKS       XXST              2
     C*
     C*  Read through rate set code file
     C     RSKY          SETLL     PRS01
     C                   READ      PRS01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
      **** BEGIN ADD **** 12/04/98 *********************************************
     C     RSJRST        IFNE      *BLANKS
      **** END   ADD **** 12/04/98 *********************************************
     C     RSJRST        IFNE      XXST
     C     RSCO#         ORNE      KKCO#
     C*
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     RSCO#         KKCO#
     C*
     C*  Get canx/rein state defn record
     C     XCKY          CHAIN     PXC01                              61
     C     *IN61         DOWEQ     *OFF
     C     XCST          ANDNE     XXST
     C     XCKY          READE     PXC01                                  61
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C                   Z-ADD     XCFND         KKFND
     C                   MOVE      XCST          KKJRST
     C     XCKY1         SETGT     PXC01
     C     XCKY1         READPE    PXC01                                  61
     C                   Z-ADD     XCNOTC        XXNOTC
     C                   Z-ADD     XCADDM        XXADDM
     C                   Z-ADD     XCRFEE        XXRFEE
     C*
     C*  Write new records to canx reason code file
     C     CRKY          CHAIN     PCR01                              62
     C     *IN62         DOWEQ     *OFF
     C*
     C                   MOVE      XXST          CRST
     C                   Z-ADD     0500101       CREFDT
     C                   Z-ADD     XXNOTC        CRNOTC
     C                   Z-ADD     XXADDM        CRADDM
     C                   Z-ADD     XXRFEE        CRRFEE
     C*
     C                   WRITE     PCR01
     C*
     C     CRKY          READE     PCR01                                  62
     C                   ENDDO                                                  *IN62=*OFF
     C*
     C                   ENDIF                                                  RSJRST<>XXST
     C*
      **** BEGIN ADD **** 12/04/98 *********************************************
     C                   ENDIF                                                  RSJRST IFNE *BLANKS
      **** END   ADD **** 12/04/98 *********************************************
     C                   READ      PRS01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
      **** BEGIN ADD **** 11/30/98 *********************************************
      *
      *  DELETE OLD RECORDS FROM THE CANCELLATION REASON TABLE FILE
      *
     C     *LOVAL        SETLL     PCR01
     C                   READ      PCR01                                  62
     C     *IN62         DOWEQ     *OFF
      *
     C     CRST          IFEQ      *BLANKS
     C                   DELETE    PCR01
     C                   ENDIF                                                  CRST IFEQ *BLANKS
      *
     C                   READ      PCR01                                  62
      *
     C                   ENDDO                                                  *IN62=*OFF
      **** END   ADD **** 11/30/98 *********************************************
     C                   MOVE      *ON           *INLR
