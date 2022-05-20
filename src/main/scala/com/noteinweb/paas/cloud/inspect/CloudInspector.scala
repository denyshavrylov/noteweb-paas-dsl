package com.noteinweb.paas.cloud.inspect

import com.noteinweb.paas.cloud.{AccountCredentials, Resource}

trait CloudInspector {
  def getResources(accountCredentials: AccountCredentials, resourceTypes: List[String]): List[Resource]
}
