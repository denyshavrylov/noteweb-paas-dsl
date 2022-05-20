package com.noteinweb.paas.cloud

case class Resource(id: String, name: String,
                    resourceType: String, props: Map[String, String],
                    nested: List[Resource]);
