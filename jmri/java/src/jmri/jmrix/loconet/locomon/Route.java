import java.util.ArrayList;

public class Route{
	int num;
	ArrayList<Turnout> route = new ArrayList<Turnout>();

	public Route(int n){
		num = n;
	}

	public String[] toInstructions(){
		String[] s = new String[route.size()];
		
		for(int i=0; i<s.length; i++)
			s[i] = route.get(i).toString();

		return s;
	}

	public void addTurnout(String id, boolean dir, boolean set){
		route.add(new Turnout(dir,id,set));
	}

	public class Turnout{
		boolean direction;
		String trackID;
		boolean set;
		public Turnout(boolean dir, String tid, boolean set){
			direction = dir;
			trackID = tid;
			this.set = set;
		}
		public String toString(){
			return "turn " + (direction ? "fwd " : "bkw ") + trackID + " " + (set ? "set" : "unset");
		}
	}
}