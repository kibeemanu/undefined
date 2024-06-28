import Debug "mo:base/Debug";
import Hash "mo:base/Hash";
import Time "mo:base/Time";
import Array "mo:base/Array";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Int "mo:base/Int";

actor TrustSystem {
  stable var products : [Product] = [];
  stable var orders : [Order] = [];
  stable var balances : [(Principal, Nat)] = [];

  type Product = {
    id : Nat;
    name : Text;
    description : Text;
    price : Nat;
    supplier : Principal;
    imageHash : Hash.Hash;
    verified : Bool;
  };

  type Order = {
    id : Nat;
    productId : Nat;
    client : Principal;
    var status : OrderStatus;
    timestamp : Time.Time;
  };

  type OrderStatus = {#Pending; #Confirmed; #Refunded};

  var nextProductId : Nat = 0;
  var nextOrderId : Nat = 0;

  public shared({caller}) func addProduct(name: Text, description: Text, price: Nat, imageHash: Hash.Hash, verified: Bool) : async Nat {
    let product : Product = {
      id = nextProductId;
      name = name;
      description = description;
      price = price;
      supplier = caller;
      imageHash = imageHash;
      verified = verified;
    };
    products := Array.tabulate<Product>(Array.size(products) + 1, func (i) {
      if (i == Array.size(products)) { product } else { products[i] }
    });
    nextProductId += 1;
    Debug.print("Product added: " # name);
    product.id
  };

  public shared({caller}) func placeOrder(productId: Nat) : async Bool {
    let productOpt = findProduct(productId);
    switch (productOpt) {
      case (null) { false };
      case (?product) {
        let order : Order = {
          id = nextOrderId;
          productId = productId;
          client = caller;
          var status = #Pending;
          timestamp = Time.now();
        };
        orders := Array.tabulate<Order>(Array.size(orders) + 1, func (i) {
          if (i == Array.size(orders)) { order } else { orders[i] }
        });
        updateBalance(product.supplier, product.price);
        nextOrderId += 1;
        Debug.print("Order placed: " # Nat.toText(order.id));
        true
      };
    };
  };

  public shared({caller}) func confirmOrder(orderId: Nat) : async Bool {
    let orderOpt = findOrder(orderId);
    switch (orderOpt) {
      case (null) { false };
      case (?order) {
        if (order.client == caller) {
          order.status := #Confirmed;
          Debug.print("Order confirmed: " # Nat.toText(order.id));
          true
        } else {
          false
        };
      };
    };
  };

  public shared({caller}) func refundOrder(orderId: Nat) : async Bool {
    let orderOpt = findOrder(orderId);
    switch (orderOpt) {
      case (null) { false };
      case (?order) {
        if (order.client == caller) {
          order.status := #Refunded;
          let productOpt = findProduct(order.productId);
          switch (productOpt) {
            case (null) { false };
            case (?product) {
              let currentBalance = getBalance(product.supplier);
              if (currentBalance >= product.price) {
                updateBalance(product.supplier, 0 - product.price);
                Debug.print("Order refunded: " # Nat.toText(order.id));
                true
              } else {
                false
              };
            };
          };
        } else {
          false
        };
      };
    };
  };

  public shared({caller}) func checkBalance() : async Nat {
    getBalance(caller)
  };

  private func findProduct(productId: Nat) : ?Product {
    Array.find<Product>(products, func(p) { p.id == productId })
  };

  private func findOrder(orderId: Nat) : ?Order {
    Array.find<Order>(orders, func(o) { o.id == orderId })
  };

  private func getBalance(principal: Principal) : Nat {
    switch (Array.find<(Principal, Nat)>(balances, func(b) { b.0 == principal })) {
      case (null) { 0 };
      case (?(_, balance)) { balance };
    }
  };

  private func updateBalance(principal: Principal, amount: Int) {
    let currentBalance = getBalance(principal);
    let newBalance = Int.abs(Int.add(currentBalance, amount));
    balances := Array.filter<(Principal, Nat)>(balances, func(b) { b.0 != principal });
    balances := Array.tabulate<(Principal, Nat)>(Array.size(balances) + 1, func (i) {
      if (i == Array.size(balances)) { (principal, newBalance) } else { balances[i] }
    });
  };
};