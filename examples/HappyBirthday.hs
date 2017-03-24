
import Mezzo

rh = melody :^ a :| g :| c' :> b
            :<. g :<< g :^ a :| g :| d' :> c'
            :<. g :<< g :^ g' :| e' :| c' :| b :| a
            :<. f' :<< f' :^ e' :| c' :| d' :>. c'

chords = ph_IVI ton (dom_D_D dom_V dom_V) (ton_T_T ton ton) :+ cadence (full subdom_IV auth_V)

-- comp = (g en' :|: g sn :|: (play rh)) :-: (pad3 (r qr) :|: (prog triple $ inKey c_maj chords))
comp = pad4 (g en' :|: g sn) :|: melAccomp c_maj triple rh chords
