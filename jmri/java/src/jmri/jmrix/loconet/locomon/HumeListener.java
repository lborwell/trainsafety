package jmri.jmrix.loconet.locomon;

import java.net.*;
import java.util.Date;
import java.util.HashMap;
import java.util.Scanner;
import jmri.jmrit.roster.Roster;
import jmri.jmrit.roster.RosterEntry;
import jmri.jmrix.loconet.LnTrafficController;
import jmri.jmrix.loconet.LnTrafficListener;
import jmri.jmrix.loconet.LocoNetListener;
import jmri.jmrix.loconet.LocoNetMessage;
import jmri.jmrix.loconet.LocoNetSystemConnectionMemo;
import jmri.jmrix.loconet.LocoNetThrottle;
import jmri.jmrix.loconet.SlotManager;

public class HumeListener implements LnTrafficListener, LocoNetListener, Runnable{
    LnTrafficController ct;
    HumeSender hs;
    int portNum = 55556;
    DatagramSocket listn;
    jmri.jmrix.loconet.locomon.Llnmon llnmon = new jmri.jmrix.loconet.locomon.Llnmon();
    SlotManager sm;
    HashMap<Integer,Locomotive> locos;
    Roster roster;
    LocoNetSystemConnectionMemo m;
    
    public HumeListener(){
        //(new Thread(new HumeListener(false))).start();

        try{
            listn = new DatagramSocket(portNum);
        }catch(Exception e){ e.printStackTrace(); }
    }
    
    public void init(LocoNetSystemConnectionMemo m){
        this.m = m;
        sm = m.getSlotManager();
        locos = new HashMap<Integer,Locomotive>();
        getTrains();
    }
    
    public void getTrains(){
        roster = Roster.instance();
        for(int i=0; i<roster.numEntries(); i++){
            RosterEntry re = roster.getEntry(i);
            int addr = Integer.parseInt(re.getDccAddress());
            //int addr = re.getDccLocoAddress();
            Locomotive l = new Locomotive(m,addr);
            System.out.println(l.toString());
            System.out.println(locos.size());
            locos.put(Integer.valueOf(l.addr), l);
            System.out.println(locos.get(Integer.valueOf(l.addr)).toString());
            sm.slotFromLocoAddress(addr, l);
        }
        printRoster();
    }
    
    void printRoster(){
        for(int k : locos.keySet())
            System.out.println("Key: " + k);
        Locomotive l = locos.get(2);
    }
    
    @Override
    public void notifyXmit(Date timestamp, LocoNetMessage m) {
    }

    @Override
    public void notifyRcv(Date timestamp, LocoNetMessage m) {
    }

    @Override
    public void message(LocoNetMessage msg) {
    }

    @Override
    public void run() {
        while(true){
            byte[] buff = new byte[255];
            DatagramPacket p = new DatagramPacket(buff,buff.length);
            String rec = "null";
            try{
                listn.receive(p);
                
                rec = new String(buff,0,buff.length);
                Scanner s = new Scanner(rec);
                s.useDelimiter(" ");
                System.out.println("Scanning on string: " + rec);
                
                int i = s.nextInt();
                if(i == 0){
                    //train
                    Locomotive l = locos.get(Integer.valueOf(s.nextInt()));
                    LocoNetThrottle t = l.getThrottle();
                    t.setSpeedSetting(t.floatSpeed(s.nextInt()));
                }else if(i==1){
                    //reverse train
                    Locomotive l = locos.get(Integer.valueOf(s.nextInt()));
                    LocoNetThrottle t = l.getThrottle();
                    t.setIsForward(!t.getIsForward());
                }else if(i==2){
                    //turnout
                }
                
                /*Locomotive l = locos.get(Integer.valueOf(2))
                System.out.println(l.toString());
                LocoNetThrottle t = l.getThrottle();
                t.setSpeedSetting(0.5f);*/
            }catch(Exception e){ e.printStackTrace(); }
            System.out.println("Received: " + rec);
        }
    }
}
