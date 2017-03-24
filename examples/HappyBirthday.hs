
import Mezzo

rh = melody :^ a :| g :| c' :> b
                    :<. g :<< g :^ a :| g :| d' :> c'
                    :<. g :<< g :^ g' :| e' :| c' :| b :| a
                    :<. f' :<< f' :^ e' :| c' :| d' :>. c'

chords = ph_IVI ton (dom_D_D dom_V dom_V) (ton_T_T ton ton) :+ cadence (full subdom_IV auth_V)

-- comp = hom (rh1 :|: rh2) (pad3 (r qr) :|: chords)
comp = pad4 (g en' :|: g sn) :|: melAccomp c_maj triple rh chords
