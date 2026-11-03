      // @MBRTEXT: Get next seq number from MBRSEQNO data area
      *================================================================
      *
      *  Program........: NXTSEQ
      *  Written by.....: M. Pryor
      *  Written for....: Variety Wholesalers, Inc.
      *  Date written...: April 30, 2001
      *  Compiler.......: RPG/400
      *
      *  Purpose........: Retrieve next available sequence number from
      *                   data area, MBRSEQNO.  The sequence number
      *                   will be used to create a unique member name
      *                   for JDA Interface files.
      *
      *================================================================

      * codermake: compile options from the PRFDEMOLIB README (CRTBNDRPG
      * DFTACTGRP(*NO) ACTGRP(*CALLER)) carried in the source.
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     DNextSeq          DS
     D  SeqNum                 1      7S 0 INZ(0)

     C     *DTAARA       DEFINE    MBRSEQNO      SeqNum

     C     *ENTRY        PLIST
     C                   PARM                    NextNo            7

     C     *LOCK         IN        SeqNum
     C                   ADD       1             SeqNum
     C                   MOVE      NextSeq       NextNo
     C                   OUT       SeqNum
     C                   MOVE      *ON           *INLR
     C                   RETURN