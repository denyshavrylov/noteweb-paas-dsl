package com.noteinweb.paas.cloud.act

import com.noteinweb.paas.cloud.CloudTypes.CloudType

trait CloudActor {
  def process(request: Request): Response;
  def getCloudType : CloudType
}
