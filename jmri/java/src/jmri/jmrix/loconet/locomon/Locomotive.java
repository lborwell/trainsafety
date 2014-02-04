/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package jmri.jmrix.loconet.locomon;

import jmri.jmrix.loconet.LocoNetSlot;
import jmri.jmrix.loconet.LocoNetSystemConnectionMemo;
import jmri.jmrix.loconet.LocoNetThrottle;
import jmri.jmrix.loconet.SlotListener;

/**
 *
 * @author Luke
 */
public class Locomotive implements SlotListener{
    public HumeListener hl;
    public LocoNetSlot s;
    public LocoNetThrottle t;
    LocoNetSystemConnectionMemo m;
    public int addr;
    
    public Locomotive(LocoNetSystemConnectionMemo m, int addr, HumeListener hl){
        this.hl = hl;
        this.m = m;
        this.addr = addr;
    }
    
    public void notifyChangedSlot(LocoNetSlot s) {
        this.s=s;
        //hl.locos.put(s.getSlot(), this);
    }
    
    public void setThrottle(LocoNetThrottle t){
        this.t = t;
    }
    
    public String toString(){
        return "Loco Addr: " + addr;
    }
    
    public LocoNetThrottle getThrottle(){
        if(s==null)
            m.getSlotManager().slotFromLocoAddress(addr, this);
        if(t==null && s != null){
            t = new LocoNetThrottle(m,s);
            System.out.println("creating throttle");
        }
        if(s==null)
            System.out.println("s null");
        
        return t;
    }
    
}
