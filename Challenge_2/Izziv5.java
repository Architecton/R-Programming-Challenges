import java.util.Scanner;
import java.text.DecimalFormat;

public class Izziv5 {

	public static void main(String[] args) {
		
		// Inicializacija instance Scanner
		Scanner sc = new Scanner(System.in);
		
		// Inicializacija kompleksnih stevil za racunanje in rezultat
		Complex fst = new Complex();
		Complex snd = new Complex();
		Complex res;
		
		// preberi prvi ukaz
		System.out.println("Vtipkaj ukaz (+, -, *, /, w):");
		String instruction = sc.nextLine();
		
		// izvrsi ukaz
		while(!instruction.equals("izhod")) {
			switch(instruction) {
				case "+":
					fst.parse(sc.next());
					snd.parse(sc.next());
					res = fst.add(snd);
					System.out.print("Rezultat: ");
					res.print();
					System.out.println();
					break;
				case "-":
					fst.parse(sc.next());
					snd.parse(sc.next());
					snd.re = -snd.re;
					snd.im = -snd.im;
					res = fst.add(snd);
					System.out.print("Rezultat: ");
					res.print();
					System.out.println();
					break;
				case "*":
					fst.parse(sc.next());
					snd.parse(sc.next());
					res = fst.multiply(snd);
					System.out.print("Rezultat: ");
					res.print();
					System.out.println();
					break;
				case "/":
					fst.parse(sc.next());
					snd.parse(sc.next());
					res = fst.divide(snd);
					System.out.print("Rezultat: ");
					res.print();
					System.out.println();
					break;
				case "w":
					int n = sc.nextInt();
					Complex.n_root(n);
					System.out.println();
					break;
				default:
					throw new Error("Neznan ukaz");
			}
			System.out.println("Vtipkaj ukaz (+, -, *, /, w):");
			instruction = sc.next();
		}
		sc.close();
	}
}

class Complex {
	// realna in kompleksna vrednosts
	double re;
	double im;
	
	// konstructor
	public Complex(double re, double im) {
		this.re = re;
		this.im = im;
	}
	
	// konstructor - blank complex number
	public Complex() {
		this.re = 0;
		this.im = 0;
	}
	
	// parse: inicializiraj kompleksno stevilo iz podanega niza
	public void parse(String input) {
		input = input.replaceAll("[^\\-.0123456789]", " ");
		input = input.replaceAll("-", " -");
		input = input.trim();
		String[] split = input.split(" ");
		this.re = Double.parseDouble(split[0]);
		this.im = Double.parseDouble(split[1]);
	}
	
	// add: sestej to stevilo s stevilom a in vrni rezultat
	public Complex add(Complex a) {
		return new Complex(this.re + a.re, this.im + a.im);
	}
	
	// multiply: zmnozi to kompleksno stevilo s kompleksnim stevilom a in vrni rezultat
	// glej vzorec
	public Complex multiply(Complex a) {
		return new Complex(this.re * a.re - this.im * a.im, this.re * a.im + this.im * a.re);
	}
	
	// multiply: zmnozi to kompleksno stevilo s kompleksim stevilom a in vrni rezultat
	public Complex multiply(double a) {
		return new Complex(this.re * a, this.im * a);
	}
	
	// multiply: deli to kompleksno stevilo s kompleksnim stevilom a in vrni rezultat
	public Complex divide(Complex a) {
		// stevec je enak this krat konjugirana vrednost a
		Complex numerator = new Complex(this.re * a.re - this.im * (-a.im), this.re * (-a.im) + this.im * a.re);
		// (a + bi)(a - bi) = a^2 + b^2
		double divisor = Math.pow(a.re, 2) + Math.pow(a.im, 2);
		// pomnozi stevec z inverzom imenovalca
		return numerator.multiply(1/divisor);
	}
	
	// n_root: izracunaj in izpisi n-ti primitivni koren enote in vse njegove potence
	public static Complex n_root(int n) {
		DecimalFormat format = new DecimalFormat("#.#####");
		format.setDecimalSeparatorAlwaysShown(true);
		format.setMinimumFractionDigits(1);
		Complex res;
		// pojdi cez interval [1, n]
		for(int k = 1; k <= n; k++) {
			// Izracunaj realni in kompleksni del stevila
			double re = Math.cos(((double)k/(double)n)*2*Math.PI);
			double im = Math.sin(((double)k/(double)n)*2*Math.PI);
			res = new Complex(re, im);
			// obravnavaj zaokrozitvene napake
			if(Math.abs(im) > 0.000009) {
				res.print();
			} else {
				System.out.printf("%s", format.format(re));
			}
			if(k < n) {
				System.out.print(" ");
			}
		}
		return null;
	}
	
	// print: izpisi to kompleksno stevilo
	public void print() {
		DecimalFormat format = new DecimalFormat("#.#####");
		format.setDecimalSeparatorAlwaysShown(true);
		format.setMinimumFractionDigits(1);
		if(this.im >= 0) {
			System.out.printf("%s+%si", format.format(this.re), format.format(this.im));
		} else {
			System.out.printf("%s%si", format.format(this.re), format.format(this.im));
		}
	}
	
	@Override
	public String toString() {
		String res = "real == " + this.re + "im == " + this.im;
		return res;
	}
}