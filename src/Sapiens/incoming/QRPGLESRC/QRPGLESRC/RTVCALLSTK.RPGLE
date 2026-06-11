     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
    H*====================================================================
    H/TITLE RtvCallStk - Retrieve call stack for current job
    H*====================================================================
      * Descrpition:
      *
      * This program was intended to ease the process of retrieving the
      *  current job's call stack.  The information returned are the program
      *  names and program library names that are displayed by the DSPJOB or
      *  WRKJOB command's call stack panel.  Running the command DSPJOB
      *  OPTION( *PGMSTK ) will show the information referred to above.
      *
      * Note:
      * This code was copied from an on-line source and may not be up to
      *  our coding standards.
      *
      *-- Parameters:
      *
      *    PxEntNbr   BOTH     The maximum number of call stack entries
      *                         to return in the output array.  A maximum
      *                         of 128 call stack entries can be returned.
      *
      *                        On return this parameter specifies the
      *                         actual number of call stack entries loaded
      *                         in the second parameter.
      *
      *    PxStkEnt   OUTPUT   The retrieved call stack entries are returned ed
      *                         in this parameter. Both the program name and
      *                         program library is returned for each call
      *                         stack entry as illustrated below:
      *
      *                           1           21          41
      *                           |  entry 1  |  entry 2  |  entry 3  | --
      *                           1     11    21    31    41    51
      *                           | pgm | lib | pgm | lib | pgm | lib | --
      *
      *                        The call stack entries are returned in
      *                         descending call level order.
      *
      *                        This means that the name of the caller of
      *                         this program will be returned in the first
      *                         entry, and the name of the first program
      *                         that was called in this job is found in
      *                         the last entry returned.
    H*====================================================================
     DRtvCallStk       pr                  ExtPgm('RTVCALLSTK')
     D PxEntNbr                       5p 0
     D PxStkEnt                      20a   Dim( 128 )
     DRtvCallStk       pi
     D PxEntNbr                       5p 0
     D PxStkEnt                      20a   Dim( 128 )
      *
      *-- System information:  --
     D PgmSts         SDs
     D  PsPgmNam         *Proc
      *
      *-- Global variables:
     D EntNbr          s             10i 0
     D Eix             s             10i 0
      *
      *-- API error data structure:
     D ApiError        Ds
     D  AeBytPrv                     10i 0 Inz( %Size( ApiError ))
     D  AeBytAvl                     10i 0
     D  AeExcpId                      7a
     D                                1a
     D  AeExcpDta                   128a
      *-- Retrieve call stack API parameters:
     D CsRcvVar        Ds
     D  CsBytRtn                     10i 0
     D  CsBytAvl                     10i 0
     D  CsNbrStkE                    10i 0
     D  CsOfsStkE                    10i 0
     D  CsNbrEntRtn                  10i 0
     D  CsThrId                       8a
     D  CsInfSts                      1a
     D  CsCalStk                  32767a
      *
     D CsCalStkE       Ds                  Based( pCalStkE )
     D  CsStkEntLen                  10i 0
     D  CsOfsStmIds                  10i 0
     D  CsNbrStmIds                  10i 0
     D  CsOfsPrcNam                  10i 0
     D  CsLenPrcNam                  10i 0
     D  CsRqsLvl                     10i 0
     D  CsPgmNam                     10a
     D  CsPgmLib                     10a
     D  CsMiInst                     10i 0
     D  CsModNam                     10a
     D  CsModLib                     10a
     D  CsCtlBdy                      1a
     D  CsRsv                         3a
     D  CsActGrpNbr                  10u 0
     D  CsActGrpNam                  10a
     D  CsAddInf                   4096a
      *
     D  CsStmIds                     10a   Dim( 256 )
     D  CsPrcNam                    512a
      *
     D CsJobId         Ds
     D  JiJobNam                     10a   Inz( '*' )
     D  JiUsrNam                     10a
     D  JiJobNbr                      6a
     D  JiIntId                      16a
     D  JiRsv                         2a   Inz( *Allx'00' )
     D  JiThrInd                     10i 0 Inz( 1 )
     D  JiThrId                       8a   Inz( *Allx'00' )
      *
     D RtvCalStk       Pr                  ExtPgm( 'QWVRCSTK' )
     D  RcRcvVar                  32767a
     D  RcRcvVarLen                  10i 0 Const
     D  RcRcvInfFmt                   8a   Const
     D  RcJobId                      56a   Const
     D  RcJobIdFmt                    8a   Const
     D  RcError                   32767a          Options( *VarSize )
      *
      *-- Mainline:
      /free
                if  PxEntNbr  > *Zero;

                 RtvCalStk( CsRcvVar
                            : %Size( CsRcvVar )
                            : 'CSTK0100'
                            : CsJobId
                            : 'JIDF0100'
                            : ApiError
                                      );

                 if  AeBytAvl = *Zero;
                  pCalStkE = %Addr( CsRcvVar ) + CsOfsStkE;

                  for EntNbr = 1  to CsNbrEntRtn;

                   if CsPgmNam  <> PsPgmNam;
                    Eix +=1;
                    PxStkEnt(Eix) = CsPgmNam + CsPgmLib;
                   endif;

                   if  EntNbr = PxEntNbr or
                       EntNbr = CsNbrEntRtn  or
                       EntNbr = %Elem( PxStkEnt );
                    leave;
                   endif;

                   pCalStkE += CsStkEntLen;
                  endfor;

                 endif;
                endif;

                PxEntNbr = Eix;

                *InLr =  *On;
                Return;
      /end-free
