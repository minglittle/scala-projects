/**
  * Created by developer on 3/26/17.
  */

class BankAccount {
  private var balance = 0
  def deposit(amount: Int): Unit = {
    if (amount > 0) balance = balance + amount
  }
  def withdraw(amount: Int): Unit = {
    if (amount > 0 && amount <= balance) {
      balance = balance - amount
      balance
    } else throw new Error("insufficient funds")
  }
}

