     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE TIMEDELAY - TO DELAY A SUBMITTED JOB FOR DEBUG
     H*****************************************************************
     H* DESCRIPTION: GIVES TIME TO USE ISDB FOR BATCH JOBS
     H*
     H* 01/20/00  C:62  PROJECT: NONE
     H*****************************************************************
     H** REVISIONS:
     H*
     H*****************************************************************
     H*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXTIME                 1      6  0
     D  XXHOUR                 1      2  0
     D  XXMINS                 3      4  0
     D  XXSECS                 5      6  0

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@MIN             3            MINUTES
     C                   PARM                    @@SEC             3            SECONDS
     C*
     C                   MOVE      @@MIN         ##MIN             3 0
     C                   MOVE      @@SEC         ##SEC             3 0
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        XXTIME
     C*
     C                   ADD       ##MIN         XXMINS
     C*
     C     XXMINS        IFGE      60
     C*
     C                   SUB       60            XXMINS
     C                   ADD       1             XXHOUR
     C*
     C     XXHOUR        IFGE      24
     C*
     C                   SUB       24            XXHOUR
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ADD       ##SEC         XXSECS
     C*
     C     XXSECS        IFGE      60
     C                   SUB       60            XXSECS
     C                   ADD       1             XXMINS
     C*
     C     XXMINS        IFGE      60
     C*
     C                   SUB       60            XXMINS
     C                   ADD       1             XXHOUR
     C*
     C     XXHOUR        IFGE      24
     C*
     C                   SUB       24            XXHOUR
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     DSTIME        DOWLE     XXTIME
     C*
     C                   TIME                    DSDTTM
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
