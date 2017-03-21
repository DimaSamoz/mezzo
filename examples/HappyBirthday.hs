
import Mezzo

rh1 = play $ melody :<. g :<< g :^ a :| g :| c' :> b :<. g :<< g :^ a :| g :| d' :> c'
rh2 = play $ melody :<. g :<< g :^ g' :| e' :| c' :| b :| a :<. f' :<< f' :^ e' :| c' :| d' :>. c'

chords = prog triple $ inKey (key _c _na majMode) $ ph_IVI ton (dom_D_D dom_V dom_V) (ton_T_T ton ton) :+ cadence (full subdom_ii auth_V7)

comp = (rh1 :|: rh2) :-: (pad3 (r qr) :|: chords)
