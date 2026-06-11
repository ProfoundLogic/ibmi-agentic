     *  (C) Copyright Progressive Data Solutions Inc., 2001
    H*********************************************************************
    H/TITLE DA001R - Extract values from key
    H*********************************************************************
    H*  Created: 02/01/01     C:33    Project: DR143                     *
    H*********************************************************************
     Fsmkdl     if   e           k disk    rename(pkd01:pkd00)
    F*
     D lo              c                   const('abcdefghijklmnopqrstuvwxyz')
     D up              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     D #ofGrpNms       c                   4
     D #ofFields       c                   8
    D*
     D grpNames        s              8    dim(#ofGrpNms) ctdata perrcd(1)
    D*
     D                 ds
     D headers                 1     64    dim(#ofFields)
     D  KDHD01                 1      8
     D  KDHD02                 9     16
     D  KDHD03                17     24
     D  KDHD04                25     32
     D  KDHD05                33     40
     D  KDHD06                41     48
     D  KDHD07                49     56
     D  KDHD08                57     64
    D*
     D                 ds
     D startPos                1     40  0 dim(#ofFields)
     D  KDDS01                 1      5  0
     D  KDDS02                 6     10  0
     D  KDDS03                11     15  0
     D  KDDS04                16     20  0
     D  KDDS05                21     25  0
     D  KDDS06                26     30  0
     D  KDDS07                31     35  0
     D  KDDS08                36     40  0
    D*
     D                 ds
     D length                  1     40  0 dim(#ofFields)
     D  KDDL01                 1      5  0
     D  KDDL02                 6     10  0
     D  KDDL03                11     15  0
     D  KDDL04                16     20  0
     D  KDDL05                21     25  0
     D  KDDL06                26     30  0
     D  KDDL07                31     35  0
     D  KDDL08                36     40  0
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@pgm           s             10
     D @@key           s             40
     D @@field         s             40
     D xxfield         s                   like(@@field)
     D header#         s              5  0
     D x               s              5  0
     D z               s                   like(header#)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     DgetGroup         PR             3
     D*
     DscanString       PR             1    opdesc
     D inString                   32766    options(*varsize) const
     D findString                 32766    options(*varsize) const
    D*____________________________________________________________________
    C*
     C     *entry        plist
     C                   parm                    @@pgm
     C                   parm                    @@key
     C                   parm                    @@field
    C*
     C* @@pgm & @@key are passed in from the DARTS remark.
     C* @@field is passed in as the value that needs to be returned
     C*    - @@field = 'Group'; returns the the group, if it exists
     C* @@field will return the value if exists, otherwise blank
    C*____________________________________________________________________
    C*
     C     lo:up         xlate     @@pgm         @@pgm
     C     lo:up         xlate     @@field       xxfield
    C*
     C                   clear                   @@field
     C                   clear                   header#
     C                   clear                   headers
     C                   clear                   startPos
     C                   clear                   length
    C*____________________________________________________________________
    C*
     C     @@pgm         chain     pkd00                              60
    C*
     C                   if        *in60
     C                   clear                   @@pgm
     C     @@pgm         chain     pkd00                              60
     C                   endif
     C*
     C                   if        not *in60
     C                   exsr      process
     C                   endif
    C*____________________________________________________________________
     C*
     C                   return
    C*____________________________________________________________________
     C*
    C*====================================================================
    C* process - Process the fields
    C*====================================================================
     C*
     C     process       begsr
     C*
     C                   select
     C                   when      xxfield = 'GROUP'
     C                   eval      @@field = getGroup
     C                   endsl
     C*
     C                   endsr





    P*====================================================================
    P* getGroup - Get the group # from the key
    P*====================================================================
    P*
     PgetGroup         b
    P*
     DgetGroup         PI             3
     D*
     D group           s                   like(getGroup)
     C*
     C                   clear                   group
     C*
     C                   do        #ofGrpNms     x
     C*
     C                   do        #ofFields     z
     C*
     C                   if        scanString(headers(z):grpNames(x)) = *on
     C                   eval      header# = z
     C                   leave
     C                   endif
     C*
     C                   enddo
     C*
     C                   if        header# <> *zero
     C                   leave
     C                   endif
     C*
     C                   enddo
     C*
     C                   if        header# <> *zero
     C                   eval      group = %subst(@@key:startPos(header#):
     C                                                    length(header#))
     C                   endif
     C*
     C                   testn                   group                99        numeric testing
     C*
     C                   if        not *in99
     C                   clear                   group
     C                   endif
     C*
     C                   return    group
     C*
     PgetGroup         e





    P*====================================================================
    P* scanString - Scan string 1 for string 2
    P*====================================================================
    P*
     PscanString       b
    P*
     DscanString       PI             1    opdesc
     D inString                   32766    options(*varsize) const
     D findString                 32766    options(*varsize) const
     D
     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     D
     DQCLSCAN          PR                  extpgm('QCLSCAN')
     D q1string                            like(inString)   const
     D                                3  0                  const
     D                                3  0                  const
     D q2string                            like(findString) const
     D                                3  0                  const
     D                                1                     const
     D                                1                     const
     D                                1                     const
     D                                3  0
     D*
     D* parameters passed to CEEDOD
     D*
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0
     D*
     D* parameters passed to QCLSCAN
     D*
     D ##nres          s              3  0
     D*
     D inStr           s                   like(inString)
     D findStr         s                   like(findString)
     D scanLen         s                   like(inLen)
     D findLen         s                   like(inLen)
     D found           s              1
    C*
     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:scanLen:*omit)
    C*
     C                   callp     CEEDOD(2:descType:dataType:descInfo1:
     C                             descInfo2:findLen:*omit)
    C*
     C     lo:up         xlate(p)  inString      inStr
     C     lo:up         xlate(p)  findString    findStr
    C*
     C                   callp     QCLSCAN(inStr:scanLen:1:
     C                                     findStr:findLen:
     C                                     '0':'1':' ':##nres)
    C*
     C                   eval      found = ##nres <> 0
    C*
     C                   return    found
     C*
     PscanString       e





** Column headings
Fnd
Fund
Grp
Group
