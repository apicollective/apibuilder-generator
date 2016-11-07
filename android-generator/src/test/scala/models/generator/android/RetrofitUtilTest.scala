package models.generator.android

import org.scalatest.{Matchers, FlatSpec}
import RetrofitUtil._

class RetrofitUtilTest
  extends FlatSpec
  with Matchers {

  "toRetrofitPath" should "convert correctly" in {

    toRetrofitPath("address/address/suggestion") should be("address/address/suggestion")
    toRetrofitPath("/address/address/suggestion") should be("address/address/suggestion")
    toRetrofitPath("admin/admin/orders") should be("admin/admin/orders")
    toRetrofitPath("admin/admin/orders") should be("admin/admin/orders")
    toRetrofitPath("/checkout_session/checkout") should be("checkout_session/checkout")
    toRetrofitPath("checkout_session/checkout/:guid") should be("checkout_session/checkout/{guid}")
    toRetrofitPath("/checkout_session/checkout/:guid") should be("checkout_session/checkout/{guid}")
    toRetrofitPath("checkout_session/checkout/:guid") should be("checkout_session/checkout/{guid}")
    toRetrofitPath("checkout_session/checkout/user/:user_guid") should be("checkout_session/checkout/user/{user_guid}")
    toRetrofitPath("checkout_session/checkout") should be("checkout_session/checkout")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/order/shipping_address") should be("checkout_session/checkout/{checkout_guid}/order/shipping_address")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/order/fulfillment") should be("checkout_session/checkout/{checkout_guid}/order/fulfillment")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/order") should be("checkout_session/checkout/{checkout_guid}/order")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/order/payment_methods") should be("checkout_session/checkout/{checkout_guid}/order/payment_methods")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/order/discounts") should be("checkout_session/checkout/{checkout_guid}/order/discounts")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/order/discounts") should be("checkout_session/checkout/{checkout_guid}/order/discounts")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/order/available_credits") should be("checkout_session/checkout/{checkout_guid}/order/available_credits")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/order/currency") should be("checkout_session/checkout/{checkout_guid}/order/currency")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/items/:sku_id") should be("checkout_session/checkout/{checkout_guid}/items/{sku_id}")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/items/:sku_id") should be("checkout_session/checkout/{checkout_guid}/items/{sku_id}")
    toRetrofitPath("/checkout_session/checkout/:checkout_guid/items/:sku_id") should be("checkout_session/checkout/{checkout_guid}/items/{sku_id}")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/user/addresses") should be("checkout_session/checkout/{checkout_guid}/user/addresses")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/user/addresses/:address_guid") should be("checkout_session/checkout/{checkout_guid}/user/addresses/{address_guid}")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/user/addresses/:address_guid") should be("checkout_session/checkout/{checkout_guid}/user/addresses/{address_guid}")
    toRetrofitPath("checkout_session/checkout/:checkout_guid/user/payment_methods/:payment_method_guid/default") should be("checkout_session/checkout/{checkout_guid}/user/payment_methods/{payment_method_guid}/default")
    toRetrofitPath("countries/countries") should be("countries/countries")
    toRetrofitPath("countries/countries/:country_code/states") should be("countries/countries/{country_code}/states")
    toRetrofitPath("currencies/currencies") should be("currencies/currencies")
    toRetrofitPath("order/orders/:order_guid") should be("order/orders/{order_guid}")
    toRetrofitPath("order/orders/:order_guid") should be("order/orders/{order_guid}")
    toRetrofitPath("order/orders/rsvps") should be("order/orders/rsvps")
    toRetrofitPath("payment_method/payment_methods/:payment_method_type") should be("payment_method/payment_methods/{payment_method_type}")
    toRetrofitPath("payment_method/payment_methods/:payment_method_type/authentication") should be("payment_method/payment_methods/{payment_method_type}/authentication")
    toRetrofitPath("payment_method/payment_methods/:payment_method_type/confirmation") should be("payment_method/payment_methods/{payment_method_type}/confirmation")
    toRetrofitPath("status/_internal_/ping") should be("status/_internal_/ping")
    toRetrofitPath("status/_internal_/status") should be("status/_internal_/status")
    toRetrofitPath("status/_internal_/version") should be("status/_internal_/version")
    toRetrofitPath("validation_rules/validation_rules") should be("validation_rules/validation_rules")
    toRetrofitPath("waitlist/waitlist") should be("waitlist/waitlist")


  }

}
