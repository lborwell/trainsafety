/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package jmri.jmrix.loconet.locomon;

import jmri.jmrix.loconet.LocoNetSystemConnectionMemo;

/**
 *
 * @author Luke
 */ 
public class HumeGod {
    public HumeGod(LocoNetSystemConnectionMemo m){
        HumeListener hl = new HumeListener();
        hl.init(m);
        (new Thread(hl)).start();
        HumeSender hs = new HumeSender();
        hs.init(m);
    }
}
