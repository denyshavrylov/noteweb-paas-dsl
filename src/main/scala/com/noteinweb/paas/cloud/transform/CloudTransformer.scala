package com.noteinweb.paas.cloud.transform

import com.noteinweb.paas.cloud.CloudTypes.CloudType
import com.noteinweb.paas.cloud.Resource

trait CloudTransformer {
  def deploy(resources: List[Resource]) : List[String]
  def undeploy(resources: List[Resource]) : List[String]
  def getCloudType : CloudType
}
