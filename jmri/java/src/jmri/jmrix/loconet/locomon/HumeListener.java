package jmri.jmrix.loconet.locomon;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.HashMap;
import java.util.Scanner;
import jmri.jmrit.roster.Roster;
import jmri.jmrit.roster.RosterEntry;
import jmri.jmrix.loconet.LnTrafficController;
import jmri.jmrix.loconet.LocoNetSystemConnectionMemo;
import jmri.jmrix.loconet.LocoNetThrottle;
import jmri.jmrix.loconet.SlotManager;

public class HumeListener implements Runnable{
    LnTrafficController ct;
    jmri.jmrix.loconet.locomon.Llnmon llnmon = new jmri.jmrix.loconet.locomon.Llnmon();
    SlotManager sm;
    HashMap<Integer,Locomotive> locos;
    Roster roster;
    LocoNetSystemConnectionMemo m;
    Socket inSock;

    BufferedReader instream;
    public HumeListener(LocoNetSystemConnectionMemo m, Socket s){
        inSock = s;
        
        try{
            instream = new BufferedReader(new InputStreamReader(inSock.getInputStream()));
        }catch(Exception e){ e.printStackTrace(); }
        
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
            locos.put(Integer.valueOf(l.addr), l);
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
    public void run() {
        while(true){
            String rec = null;
            try{
                while(rec == null)
                    rec = instream.readLine();
                
                Scanner s = new Scanner(rec);
                s.useDelimiter(" ");
                
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
            }catch(Exception e){ e.printStackTrace(); }
            System.out.println("Received: " + rec);
            rec = null;
        }
    }
}
