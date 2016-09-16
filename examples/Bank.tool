program Bank {
    println (new Simulation().init());
}

class Simulation {
	var bankMoney : Int;
	var taxReceived : Int;
	var client1 : Client;
	var client2 : Client;
	var client3 : Client;
	var client4 : Client;
	
	def init() : String = {
		println("---|| Simulation started ||---");
		bankMoney = 0;
		client1 = new Client();
		client2 = new Client();
		client3 = new Client();
		client4 = new Client();
		return this.start();
	}
	
	def start() : String = {
	
		println(client1.init("Bob", 185541));
		println(client2.init("Douchy", 10));
		println(client3.init("Brian", 654));
		println(client4.init("Doug", 1254635));
		
		//First Year
		println(client2.transfer(client3, 50));
		bankMoney = bankMoney + this.collectTaxes();
		
		//Second Year
		println(client1.transfer(client3, 4500));
		println(client1.transfer(client2, 1500));
		bankMoney = bankMoney + this.collectTaxes();
		
		//Third Year
		println(client4.transfer(client1, 100000));
		bankMoney = bankMoney + this.collectTaxes();
		
		//Fourth Year
		println(client1.transfer(client3, 25000));
		println(client2.transfer(client3, 500));
		bankMoney = bankMoney + this.collectTaxes();
		
		println(client1.report());
		println(client2.report());
		println(client3.report());
		println(client4.report());
		
		println("Bank has earned " + bankMoney + "$.");
		return "---|| End of Simulation ||---";
	}
	
	def collectTaxes() : Int = {
		taxReceived = 0;
		taxReceived = taxReceived + client1.taxes();
		taxReceived = taxReceived + client2.taxes();
		taxReceived = taxReceived + client3.taxes();
		taxReceived = taxReceived + client4.taxes();
		return taxReceived;
	}
}

// Useless comment
class Client {
	var clientName : String;
	var totalMoney : Int;
	var message : String;
	var tax : Int;
	var amountOwed : Int;
	
	var creditorName : String;
	
	def init(name : String, money : Int) : String = {
		clientName = name;
		totalMoney = money;
		message = this.report();
		return message;
	}
	
	def transfer(sc : Client, amount: Int) : String = {
		if (amount < 0){
			message = "Impossible to execute transfer, " + amount + " is negative.";
		} else {
			if (totalMoney < amount){
				message = clientName + " has not enough money.";
			} else {
				creditorName = sc.getName();
				totalMoney = totalMoney  - amount;
				println(sc.receiveMoney(amount));
				message = amount + "$ have been transfered from " + clientName + " to " + creditorName + ".";
			}
		}
		return message;
	}
	
	def taxes() : Int = {
		tax = totalMoney / 100;		// Rounded to the greatest inferior integer
		totalMoney = totalMoney - tax;
		println(clientName + " paid " + tax + "$ of taxes and now has " + totalMoney + "$.");
		return tax;
	}
	
	def report() : String = {
		if(totalMoney < 0){
			amountOwed = 0 - totalMoney;
			message = clientName + " owes the bank " + amountOwed + ".";
		} else {
			message  = clientName + " has " + totalMoney + "$ in his account.";
		}
		return message;
	}
	
	def getName(): String = {
		return clientName;
	}
	
	def getTotalMoney(): Int = {
		return totalMoney;
	}
	
	def receiveMoney(amount : Int): String = {
		totalMoney = totalMoney + amount;
		return clientName + " received " + amount + "$.";
	}
}
