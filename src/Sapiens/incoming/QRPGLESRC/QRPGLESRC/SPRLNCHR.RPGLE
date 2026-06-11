     Dhandle           s             50
     DsvrAddr          s             30
     Dccsid            s             10
     Dcommand          s             10
     Doption           s              1
     Dparameter1       s            512
     Dparameter2       s           1024
     Dresult           s            512

     DaddBookMrk       pr
     D name                                like(parameter1) const

     DcheckFIle        pr              n
     D file                                like(parameter1) value

     DcloseLnchr       pr

     DcloseWord        pr

     DendComm          pr

     DgetDftSvr        pr            10

     DgetGenDir        pr            50

     DgetNetSvr        pr            20

     DgetQDLSDir       pr            50

     DgetShlDir        pr            50

     DinsertFile       pr
     D fileName                            like(parameter1) const
     DinsertText       pr
     D text                                like(parameter1) const

     DnewDocument      pr
     D template                            like(parameter1) const

     DopenAsRead       pr
     D document                            like(parameter1) const

     DopenFile         pr
     D document                            like(parameter1) const

     DopenLnchr        pr
     D opnHndle                            like(handle) const
     D                                     options(*nopass)
     D server                              like(svrAddr) const
     D                                     options(*nopass)
     D sid                                 like(ccsid) const
     D                                     options(*nopass)

     DopenWord         pr

     DpageBreak        pr

     DprintWord        pr
     D                               10    value
     D                                3  0 value

     DrplBookMrk       pr
     D name                                like(parameter1) const
     D value                               like(parameter2) const
     D                                     options(*nopass)

     DsaveAs           pr
     D saveName                            like(parameter1) const

     DsaveFile         pr

     DsendDoc          pr
     D recipients                          like(parameter1) const
     DshowWord         pr

     DendMail          pr

     DsendMail         pr

     DstartMail        pr
     D recipients                          like(parameter1) const
     D subject                             like(parameter2) const
