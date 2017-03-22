
import Mezzo

rh1 = play $ melody :<. g :<< g :^ a :| g :| c' :> b :<. g :<< g :^ a :| g :| d' :> c'
rh2 = play $ melody :<. g :<< g :^ g' :| e' :| c' :| b :| a :<. f' :<< f' :^ e' :| c' :| d' :>. c'

chords = prog triple $ inKey c_min $ ph_IVI ton (dom_D_D dom_V dom_V) (ton_T_T ton ton) :+ cadence (full subdom_IV auth_V)

comp = hom (rh1 :|: rh2) (pad3 (r qr) :|: chords)
