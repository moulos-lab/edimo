.defineUsersCollection <- function() {
    return(
    '{
      "create": "users",
      "validator": {
        "$jsonSchema": {
          "bsonType": "object",
          "required": [
            "_id",
            "username",
            "password",
            "metadata"
          ],
          "properties": {
            "_id": {
              "bsonType": "objectId"
            },
            "username": {
              "bsonType": "string"
            },
            "password": {
              "bsonType": "object",
              "required": [
                "bcrypt"
              ],
              "properties": {
                "bcrypt": {
                  "bsonType": "string"
                }
              }
            },
            "metadata": {
              "bsonType": "object",
              "required": [
                "date_created",
                "date_updated",
                "last_login"
              ],
              "properties": {
                "date_created": {
                  "bsonType": "date"
                },
                "date_updated": {
                  "bsonType": [
                    "date",
                    "null"
                  ]
                },
                "last_login": {
                  "bsonType": [
                    "date",
                    "null"
                  ]
                },
                "last_login_attempt": {
                  "bsonType": [
                    "date",
                    "null"
                  ]
                },
                "login_attempts": {
                  "bsonType": "int"
                },
                "role": {
                  "bsonType": "string"
                },
                "account_locked": {
                  "bsonType": "bool"
                }
              }
            },
            "emails": {
              "bsonType": "array",
              "items": {
                "bsonType": "object",
                "properties": {
                  "address": {
                    "bsonType": "string"
                  },
                  "verified": {
                    "bsonType": "bool"
                  },
                  "main": {
                    "bsonType": "bool"
                  },
                  "verification_token": {
                    "bsonType": "string"
                  }
                }
              }
            },
            "profile": {
              "bsonType": "object",
              "properties": {
                "name": {
                  "bsonType": "string"
                },
                "surname": {
                  "bsonType": "string"
                },
                "sex": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "dob": {
                  "bsonType": [
                    "date",
                    "null"
                  ]
                },
                "phone": {
                  "bsonType": "object",
                  "properties": {
                    "fix": {
                      "bsonType": [
                        "string",
                        "null"
                      ]
                    },
                    "mobile": {
                      "bsonType": [
                        "string",
                        "null"
                      ]
                    }
                  }
                },
                "address": {
                  "bsonType": "object",
                  "properties": {
                    "institution": {
                      "bsonType": [
                        "string",
                        "null"
                      ]
                    },
                    "street": {
                      "bsonType": [
                        "string",
                        "null"
                      ]
                    },
                    "city": {
                      "bsonType": [
                        "string",
                        "null"
                      ]
                    },
                    "state": {
                      "bsonType": [
                        "string",
                        "null"
                      ]
                    },
                    "zip": {
                      "bsonType": [
                        "string",
                        "null"
                      ]
                    },
                    "country": {
                      "bsonType": "string"
                    }
                  }
                }
              }
            }
          },
          "additionalProperties": false
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      }
    }'
    )
}

.defineLogsCollection <- function() {
    return(
    '{
      "create": "logs",
      "validator": {
        "$jsonSchema": {
          "bsonType": "object",
          "required": [
            "_id",
            "timestamp",
            "level"
          ],
          "properties": {
            "_id": {
              "bsonType": "objectId"
            },
            "timestamp": {
              "bsonType": "date"
            },
            "level": {
              "bsonType": "string",
              "maxLength": 16
            },
            "caller": {
              "bsonType": [
                "string",
                "null"
              ],
              "maxLength": 128
            },
            "message": {
              "bsonType": [
                "string",
                "null"
              ]
            },
            "sys_uname": {
              "bsonType": [
                "string",
                "null"
              ],
              "maxLength": 64
            },
            "user_name": {
              "bsonType": [
                "string",
                "null"
              ],
              "maxLength": 128
            }
          },
          "additionalProperties": true
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      }
    }'
    )
}

.defineInstitutionsCollection <- function() {
    return(
    '{
      "create": "institutions",
      "validator": {
        "$jsonSchema": {
          "bsonType": "object",
          "required": [
            "_id",
            "profile",
            "tracking"
          ],
          "properties": {
            "_id": {
              "bsonType": "objectId"
            },
            "profile": {
              "bsonType": "object",
              "required": [
                "name",
                "title",
                "street",
                "city",
                "state",
                "zip",
                "country",
                "email",
                "tel",
                "contact_person",
                "web_page"
              ],
              "properties": {
                "name": {
                  "bsonType": "string"
                },
                "title": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "street": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "city": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "state": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "zip": {
                  "bsonType": [
                    "int",
                    "null"
                  ]
                },
                "country": {
                  "bsonType": "string"
                },
                "email": {
                  "bsonType": "string",
                  "pattern": "^[\\w.%+-]+@[\\w.-]+\\.[A-Za-z]{2,}$"
                },
                "tel": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "contact_person": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "web_page": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "notes": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                }
              }
            },
            "tracking": {
              "bsonType": "object",
              "required": [
                "date_created",
                "date_updated",
                "inserted_by",
                "edited_by"
              ],
              "properties": {
                "date_created": {
                  "bsonType": "date"
                },
                "date_updated": {
                  "bsonType": [
                    "date",
                    "null"
                  ]
                },
                "inserted_by": {
                  "bsonType": "string"
                },
                "edited_by": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                }
              }
            }
          }
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      }
    }'
    )
}

.defineStaticsCollection <- function() {
    return(
    '{
      "create": "statics",
      "validator": {
        "$jsonSchema": {
          "bsonType": "object",
          "required": [
            "_id",
            "countries",
            "sex",
            "analysis_status"
          ],
          "properties": {
            "_id": {
              "bsonType": "objectId"
            },
            "countries": {
              "bsonType": "array",
              "items": {
                "bsonType": "object",
                "required": [
                  "name_en",
                  "iso2c",
                  "iso3c",
                  "iso3n",
                  "name_el"
                ],
                "properties": {
                  "name_en": {
                    "bsonType": "string",
                    "maxLength": 128
                  },
                  "iso2c": {
                    "bsonType": "string",
                    "minLength": 2,
                    "maxLength": 2
                  },
                  "iso3c": {
                    "bsonType": "string",
                    "minLength": 3,
                    "maxLength": 3
                  },
                  "iso3n": {
                    "bsonType": "string",
                    "minLength": 3,
                    "maxLength": 3
                  },
                  "name_el": {
                    "bsonType": [
                      "string",
                      "null"
                    ],
                    "maxLength": 128
                  }
                }
              }
            },
            "sex": {
              "bsonType": "array",
              "items": {
                "bsonType": "object",
                "required": [
                  "value",
                  "name"
                ],
                "properties": {
                  "value": {
                    "bsonType": "int"
                  },
                  "name": {
                    "bsonType": "string"
                  }
                }
              }
            },
            "analysis_status": {
              "bsonType": "array",
              "items": {
                "bsonType": "object",
                "required": [
                  "value",
                  "description"
                ],
                "properties": {
                  "value": {
                    "bsonType": "int"
                  },
                  "description": {
                    "bsonType": "string"
                  }
                }
              }
            },
            "organism": {
              "bsonType": "array",
              "items": {
                "bsonType": "string"
              }
            },
            "genome_version": {
              "bsonType": "array",
              "items": {
                "bsonType": "object",
                "required": [
                  "ucsc",
                  "ensembl",
                  "description"
                ],
                "properties": {
                  "ucsc": {
                    "bsonType": "string"
                  },
                  "ensembl": {
                    "bsonType": "string"
                  },
                  "description": {
                    "bsonType": "string"
                  }
                }
              }
            },
            "sequencing_platform": {
              "bsonType": "array",
              "items": {
                "bsonType": "string"
              }
            },
            "sequencing_protocol": {
              "bsonType": "array",
              "items": {
                "bsonType": "string"
              }
            }
          },
          "additionalProperties": true
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      }
    }'
    )
}
