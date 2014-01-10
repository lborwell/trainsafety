package jmri.jmrix.loconet.locomon;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.Date;
import java.util.HashMap;
import jmri.jmrit.roster.Roster;
import jmri.jmrit.roster.RosterEntry;
import jmri.jmrix.loconet.LnTrafficController;
import jmri.jmrix.loconet.LnTrafficListener;
import static jmri.jmrix.loconet.LnTrafficListener.LN_TRAFFIC_ALL;
import jmri.jmrix.loconet.LocoNetListener;
import jmri.jmrix.loconet.LocoNetMessage;
import jmri.jmrix.loconet.LocoNetSystemConnectionMemo;
import jmri.jmrix.loconet.LocoNetThrottle;
import jmri.jmrix.loconet.SlotManager;

public class HumeSender implements LnTrafficListener, LocoNetListener{
    LnTrafficController ct;
    int portNum = 55555;
    DatagramSocket ss;
    jmri.jmrix.loconet.locomon.Llnmon llnmon = new jmri.jmrix.loconet.locomon.Llnmon();
    LocoNetSystemConnectionMemo m;
    
    public HumeSender(){
        try{
            ss = new DatagramSocket();
        }catch(Exception e){ e.printStackTrace(); }
    }
    
    public void init(LocoNetSystemConnectionMemo m){
        this.m = m;
        m.getLnTrafficController().addTrafficListener(LN_TRAFFIC_ALL, this);
        m.getLnTrafficController().addLocoNetListener(~0, this);
    }
    
    @Override
    public void notifyXmit(Date timestamp, LocoNetMessage m) {
    }

    @Override
    public void notifyRcv(Date timestamp, LocoNetMessage m) {
    }

    @Override
    public void message(LocoNetMessage msg) {
        try{
            String m = llnmon.format(msg);
            ss.send(new DatagramPacket(m.getBytes(),m.length(),InetAddress.getLocalHost(),portNum));
        }catch(Exception e){ e.printStackTrace(); }
    }
}
