     DdsWITMPLTparms   ds                  qualified inz
     D co#                            3  0
     D fnd                            3  0
     D state                          2
     D fein                           9  0
     D mod                            3  0

     DTemplateAssociations...
     D                 pr                  extpgm('WITMPLT')
     D parms                               like(dsWITMPLTparms)
     D call                          25    const options(*nopass)
     D* * * Company, Group, Group/State, Agency, Agency/Group
     D mode                           1    const options(*nopass)
     DProc_Tmplt...
     D                 pr
     D parms                               like(dsWITMPLTparms)
     D call                          25    const options(*nopass)
     D mode                           1    const options(*nopass)
