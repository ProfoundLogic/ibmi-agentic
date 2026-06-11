    H*================================================================
     H*  (C) Copyright FISERV Insurance Solutions, Inc., 2007
    H*
    H*  FIXCMCDATR - Fix Remark date in the DARTS Remarks file.
     H*
     H*  CREATED: 05/09/007 CONSULTANT: C:180  PROJECT: DR135 55193
    H*
    H*================================================================
     H* REVISIONS:                                                    *
     H*                                                               *
     FSDCMP     UP   E             DISK
      *
     C                   IF        CMCDAT = *ZEROS and CMIDT > *ZEROS
     C                   MOVE      CMIDT         CMCDAT
     C                   UPDATE    PCM01
     C                   ENDIF
      *
