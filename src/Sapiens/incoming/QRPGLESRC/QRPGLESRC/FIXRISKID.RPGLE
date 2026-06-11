     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*****************************************************************
    h/TITLE FIXRISKID - Fix program to move Risk ID # Control Flags
    H*****************************************************************
    H*                                                               *
    H*    PROGRAM DESCRIPTION:                                       *
    H*      THIS PROGRAM COPIES VALUES OF RISK ID AND NCCI FLAGS FROM*
    H*      ADMIN/DARTS FILE TO STAT RPT/RATING FILE                 *
    H*                                                               *
    H*    CALLED FROM:                                               *
    H*      COMMAND LINE - ONE TIME DATA FIX                         *
    H*                                                               *
    H*    CALLS TO:                                                  *
    H*      NONE                                                     *
    H*                                                               *
    H*    PROGRAMMER: C:191                                          *
    H*    CREATED:    5/13/09      PROJECT: WC1794                   *
    H*                                                               *
    H*****************************************************************
    H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     fwmexl     uf   e           k disk
     fwdrrl     uf   e           k disk
     f*
     d cltid           s              3    inz
     d xxfound         s              2  0 inz
     d xxBackup        s              1N   inz
     d arrclt          s              3    dim(12) ctdata perrcd(1) ascend
     d cmd             c                   const('ARCOBJ OBJ(WMEXP) OBJTYPE(*FI-
     d                                     LE)')
     c*
     c******************************************************************
     c* K E Y L I S T S
     c******************************************************************
     c     rrky          klist
     c                   kfld                    exco#
     c                   kfld                    exfnd
     c*
     c******************************************************************
     c* M A I N  R O U T I N E
     c******************************************************************
     c*
     c                   eval      *inlr = *on
     c                   eval      xxBackup = *off
     c*
     c* Return if client id is 25, 33, 48, 81, 91, 93, 96, 101, 107, 122, 129, and 131
     c                   call      'RTVCLTID'
     c                   parm                    cltid
     c*
     c                   eval      xxfound = %lookup(cltid:arrclt)
     c                   if        xxfound > 0
     c* cltid found in array of clients to skip
     c                   return
     c                   endif                                                  xxfound>0
     c*
     c* Process admin/DARTS file
     c     *loval        setll     pex01
     c                   read      pex01
     c*
     c                   dow       not %eof
     c* Atleast one of the flag should be non-blank
     c                   if        exsrrb <> *blanks or exsrrq <> *blanks or
     c                             exnrrb <> *blanks or exnrrq <> *blanks
     c* Update state rpt/rating file
     c     rrky          setll     prr01
     c     rrky          reade     prr01
     c                   dow       not %eof
     c                   eval      rrsrrb = exsrrb
     c                   eval      rrsrrq = exsrrq
     c                   eval      rrnrrb = exnrrb
     c                   eval      rrnrrq = exnrrq
     c                   update    prr01
     c     rrky          reade     prr01
     c                   enddo                                                  not %eof(prr01)
     c*
     c* Since we are here atleast one record had flags to be copied
     c* backup wmexp before updating - only once!
     c                   if        not xxBackup
     c* Archive existing file WMEXP using ARCOBJ command
     c                   call      'QCMDEXC'
     c                   parm      cmd           cmdstr           35
     c                   parm      35            length           15 5
     c*
     c                   eval      xxBackup = *on
     c                   endif                                                  not xxBackup
     c*
     c* Update admin/DARTS control file flags as blanks
     c                   eval      exsrrb = *blanks
     c                   eval      exsrrq = *blanks
     c                   eval      exnrrb = *blanks
     c                   eval      exnrrq = *blanks
     c                   update    pex01
     c                   endif                                                  exsrrb<>*blanks
     c*
     c                   read      pex01
     c                   enddo                                                  not %eof(pex01)
     c*
** # client ids to skip
025
033
048
081
091
093
096
101
107
122
129
131
