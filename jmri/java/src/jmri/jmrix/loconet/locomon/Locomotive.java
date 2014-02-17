/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package jmri.jmrix.loconet.locomon;
import jmri.InstanceManager;
import jmri.DccLocoAddress;
import jmri.DccThrottle;
import jmri.ThrottleListener;
import jmri.jmrit.roster.RosterEntry;
import jmri.jmrix.loconet.LocoNetSlot;
import jmri.jmrix.loconet.LocoNetSystemConnectionMemo;
import jmri.jmrix.loconet.LocoNetThrottle;
import jmri.jmrix.loconet.SlotListener;

/**
 *
 * @author Luke
 */
public class Locomotive implements SlotListener, ThrottleListener{
    private boolean throttleRequested = false;
    public HumeListener hl;
    public LocoNetSlot s;
    public LocoNetThrottle t;
    LocoNetSystemConnectionMemo m;
    RosterEntry re;
    public int addr;
    
    public Locomotive(LocoNetSystemConnectionMemo m, int addr, HumeListener hl, RosterEntry re){
        this.hl = hl;
        this.m = m;
        this.addr = addr;
        this.re = re;
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
    
    public synchronized LocoNetThrottle getThrottle(){
        if(t==null){
            if(throttleRequested) return null;
            boolean b = false;
            while(!b)
                b = InstanceManager.throttleManagerInstance().requestThrottle(re, this);
            throttleRequested = true;
        }
        return t;
    }

    @Override
    public synchronized void notifyThrottleFound(DccThrottle t) {
        this.t = (LocoNetThrottle) t;
    }

    @Override
    public void notifyFailedThrottleRequest(DccLocoAddress address, String reason) {
    }
    
}
