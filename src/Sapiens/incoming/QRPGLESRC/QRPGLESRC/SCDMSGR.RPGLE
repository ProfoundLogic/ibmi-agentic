     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**$******************************************************************
    H/TITLE SCDMSGR - SEND DIARY MESSAGE
    H*********************************************************************
    H*  DESCRIPTION: This program loads default message fields ot be sent*
    H*               to the 'TO' diary user from the remark code table   *
    H*               file.                                               *
    H*                                                                   *
    H*  CREATED: 11/08/99      C:33      PROJECT: DR078                  *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fsdcmla    if   e           k disk
     Fstctl1    if   e           k disk
    D*
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@key           s             40
     D @@seq#          s              5
     D @@dflt_msg      s             75
     D @@dspl_key      s             72
     D @@clmnt_nm      s             72
     D @@err           s              1
     D ##pgm_name      s                   like(CMPNAM)
     D ##dspl_key      s             79
     D ##clmnt_nm      s             79
     D ##rtcd          s              1
     D xxseq#          s                   like(CMSEQ#)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C*  COMMENT(REMARK) DETAIL FILE
    C*
     C     cmky          klist
     C                   kfld                    @@key
     C                   kfld                    xxseq#
    C*
    C*  REMARK CODE FILE
    C*
     C     ctky          klist
     C                   kfld                    CMRMCD
    C*
     C     *entry        plist
     C                   parm                    @@key
     C                   parm                    @@seq#
     C                   parm                    @@dflt_msg
     C                   parm                    @@dspl_key
     C                   parm                    @@clmnt_nm
     C                   parm                    @@err
    C*____________________________________________________________________
    C*
     C                   move      @@seq#        xxseq#
    C*
     C     cmky          chain     pcm01                              60
    C*
     C                   if        *in60 = *off
     C                   exsr      process_CM
     C                   else
     C                   eval      @@err = *in60
     C                   endif
    C*____________________________________________________________________
    C*
     C                   move      *on           *inlr
    C*____________________________________________________________________
    C*
    C*====================================================================
    C* get_dsply - Get the key & claimant name as it should be displayed
    C*====================================================================
    C*
     C     get_dsply     begsr
    C*
    C*  GET THE KEY INFO / NAME BASED ON THE PNAM
    C*
     C                   eval      ##dspl_key = @@key
    C*
     C                   call      'SRKCVT1'
     C                   parm      CMPNAM        ##pgm_name
     C                   parm                    ##dspl_key
     C                   parm      *blanks       ##clmnt_nm
     C                   parm      ' '           ##rtcd
    C*
     C                   if        ##rtcd = *blanks
     C                   eval      @@dspl_key = ##dspl_key
     C                   eval      @@clmnt_nm = ##clmnt_nm
     C                   else
     C                   eval      @@dspl_key = @@key
     C                   eval      @@clmnt_nm = *blanks
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* process_CM - Process SDCMLA record
    C*====================================================================
    C*
     C     process_CM    begsr
     C*
     C                   exsr      get_dsply
    C*
     C     ctky          chain     pct01                              60
    C*
     C                   if        *in60 = *off and CTSTAT <> 'I'
     C                   exsr      process_CT
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* process_CT - Process STCTL1 records
    C*====================================================================
    C*
     C     process_CT    begsr
    C*
     C                   if        CTDMSG <> *blanks
     C                   eval      @@dflt_msg = CTDMSG
     C                   else
     C                   eval      @@dflt_msg = *blanks
     C                   endif
    C*
     C                   endsr
