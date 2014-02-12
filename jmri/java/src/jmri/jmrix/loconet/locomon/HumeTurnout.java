/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package jmri.jmrix.loconet.locomon;

import jmri.Turnout;
import jmri.InstanceManager;
import jmri.TurnoutManager;
import jmri.jmrix.loconet.LnTurnout;

/**
 *
 * @author Luke
 */
public class HumeTurnout {
    LnTurnout t = null;
    public boolean lastRequest;
    boolean direction;
    String trackID;
    int id;
    
    public HumeTurnout(int id, boolean set, boolean direction, String tid){
        lastRequest = set;
        this.id = id;
        this.direction = direction;
        trackID = tid;
    }
    
    public synchronized void set(boolean s){
        if(t==null){
            TurnoutManager tm = InstanceManager.turnoutManagerInstance();
            t = (LnTurnout)tm.provideTurnout("LT" + id);
            System.out.println("Requesting turnout LT" + id);
        }
        t.setCommandedState(s?Turnout.THROWN:Turnout.CLOSED);
    }
    
    public synchronized String dirString(){
        return direction ? "fwd" : "bkw";
    }
    
    public synchronized String toString(){
        return "turn " + trackID + " " + (direction ? "fwd " : "bkw ") + " " + (lastRequest ? "set" : "unset");
    }
    
    public synchronized boolean sendMsg(boolean req){
        if(req==lastRequest)
            return false;
        lastRequest = req;
        return true;
    }
    
    public static String boolToSet(boolean s){
        return s ? "set" : "unset";
    }
    
    public static boolean setToBool(String s){
        return s.equals("set");
    }
}
