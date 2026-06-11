     Hcopyright('Fiserv Insurance Solutions, 2004')
     *********************************************************************
     /title WARSTNLC - Reset the Name Link Counter in WXNAP, WXNAP1 & WXNEP
     *********************************************************************
     * Description:                                                      *
     *  This program will update all records in WXNAP, WXNAP1 & WXNEP    *
     *   with the Name Link Counter equal to the value passed in.  The   *
     *   Name Link Counter will be incrimented by 1.  The value passed   *
     *   in will be incremented by 1 and returned to the calling program.*
     *                                                                   *
     * Created: 09/10/04   C:33   Projcet: WC1242                        *
     *********************************************************************
     * Revisions:                                                        *
     *                                                                   *
     *********************************************************************

     FWXNAL     UF   E           k disk    rename(pna01:pna00)
     FWXNAL12   UF   E           k disk    rename(pna01:pna12)
     FWXNEL     UF   E           k disk    rename(pne01:pne00)

     D/copy sprdate

     D*Entry
     D WARSTNLKC       pr
     D  inCo#                              like(NACO#)  const
     D  inFnd                              like(NAFND)  const
     D  inEmp#                             like(NAEMP#) const
     D  inFyr                              like(NAFYR)  const
     D  inUNID                             like(NAUNID) const
     D  inNLKC                             like(NANLKC)
     D WARSTNLKC       pi
     D  inCo#                              like(NACO#)  const
     D  inFnd                              like(NAFND)  const
     D  inEmp#                             like(NAEMP#) const
     D  inFyr                              like(NAFYR)  const
     D  inUNID                             like(NAUNID) const
     D  inNLKC                             like(NANLKC)

     D psds          esds                  extname(SZQ1P)

     D xxENCD          s                   like(NAENCD) inz(*loval)

     C     WXNALkey      klist
     C                   kfld                    inCO#
     C                   kfld                    inFND
     C                   kfld                    inEMP#
     C                   kfld                    inFYR
     C                   kfld                    inUNID
     C                   kfld                    inNLKC
     C                   kfld                    xxENCD
     C*
     C     WXNALkey1     klist
     C                   kfld                    inCO#
     C                   kfld                    inFND
     C                   kfld                    inEMP#
     C                   kfld                    inFYR
     C                   kfld                    inUNID
     C*
     C     WXNAL12key    klist
     C                   kfld                    inCO#
     C                   kfld                    inFND
     C                   kfld                    inEMP#
     C                   kfld                    inFYR
     C                   kfld                    inNLKC
     C                   kfld                    xxENCD
     C*
     C     WXNAL12key1   klist
     C                   kfld                    inCO#
     C                   kfld                    inFND
     C                   kfld                    inEMP#
     C                   kfld                    inFYR
     C*
     C     WXNELkey      klist
     C                   kfld                    inCO#
     C                   kfld                    inFND
     C                   kfld                    inEMP#
     C                   kfld                    inFYR
     C                   kfld                    inUNID
     C                   kfld                    inNLKC
     C                   kfld                    xxENCD
     C*
     C     WXNELkey1     klist
     C                   kfld                    inCO#
     C                   kfld                    inFND
     C                   kfld                    inEMP#
     C                   kfld                    inFYR
     C                   kfld                    inUNID

     C                   exsr      WXNAP
     C                   exsr      WXNAP1
     C                   exsr      WXNEP
     C                   eval      inNLKC = inNLKC + 1
     C*
     C                   eval      *inlr = *on

     *====================================================================
     * NAMaintFlds - Update maintenance fields
     *====================================================================
     C     NAMaintFlds   begsr
     C*
     C                   eval      NAUSER = q1user
     C                   eval      NAPGM  = q1pgm
     C                   eval      NADT   = DateTo7(SystemDate)
     C                   eval      NATIME = TimeTo6(SystemTime)
     C*
     C                   endsr                                                  NAMaintFlds
     C*
     *====================================================================
     * NEMaintFlds - Update maintenance fields
     *====================================================================
     C     NEMaintFlds   begsr
     C*
     C                   eval      NEUSER = q1user
     C                   eval      NEPGM  = q1pgm
     C                   eval      NEDT   = DateTo7(SystemDate)
     C                   eval      NETIME = TimeTo6(SystemTime)
     C*
     C                   endsr                                                  NAMaintFlds
     C*
     *====================================================================
     * WXNAP - Process WXNAL1 file
     *====================================================================
     C     WXNAP         begsr
     C*
     C     WXNALkey      setll     pna00
     C                   dou       %eof or NANLKC > inNLKC + 1
     C     WXNALkey1     reade     pna00
     C                   if        not %eof and
     C                             NANLKC = inNLKC and NAENCD <> 999
     C                   eval      NANLKC = NANLKC + 1
     C                   exsr      NAMaintFlds
     C                   update    pna00
     C                   endif
     C                   enddo
     C*
     C                   endsr                                                  RSTLCK
     C*
     *====================================================================
     * WXNAP1 - Process WXNAL12 file
     *====================================================================
     C     WXNAP1        begsr
     C*
     C     WXNAL12key    setll     pna12
     C                   dou       %eof or NANLKC > inNLKC + 1
     C     WXNAL12key1   reade     pna12
     C                   if        not %eof and
     C                             NANLKC = inNLKC and NAENCD <> 999
     C                   eval      NANLKC = NANLKC + 1
     C                   exsr      NAMaintFlds
     C                   update    pna12
     C                   endif
     C                   enddo
     C*
     C                   endsr                                                  RSTLCK
     C*
     *====================================================================
     * WXNEP - Process WXNEP file
     *====================================================================
     C     WXNEP         begsr
     C*
     C     WXNELkey      setll     pne00
     C                   dou       %eof or NENLKC > inNLKC + 1
     C     WXNELkey1     reade     pne00
     C                   if        not %eof and
     C                             NENLKC = inNLKC and NEENCD <> 999
     C                   eval      NENLKC = NENLKC + 1
     C                   exsr      NEMaintFlds
     C                   update    pne00
     C                   endif
     C                   enddo
     C*
     C                   endsr                                                  RSTLCK
