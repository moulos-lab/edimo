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
          "additionalProperties": false
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      },
      "validationLevel": "strict"
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
      },
      "validationLevel": "strict"
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
      },
      "validationLevel": "strict"
    }'
    )
}

.defineSamplesCollection <- function() {
    return(
    '{
      "create": "samples",
      "validator": {
        "$jsonSchema": {
          "bsonType": "object",
          "required": [
            "_id",
            "name",
            "files",
            "metadata",
            "ownership",
            "genome_size",
            "quality"
          ],
          "properties": {
            "_id": {
              "bsonType": "objectId"
            },
            "name": {
              "bsonType": "string"
            },
            "description": {
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
            },
            "files": {
              "bsonType": "array",
              "items": {
                "bsonType": "string"
              }
            },
            "metadata": {
              "bsonType": "object",
              "required": [
                "file_type",
                "original_names",
                "paired",
                "library_protocol",
                "library_kit",
                "sequencing_platform",
                "aligner",
                "variant_caller",
                "date_created",
                "date_updated",
                "insert_token"
              ],
              "properties": {
                "file_type": {
                  "bsonType": "string"
                },
                "original_names": {
                  "bsonType": "array",
                  "items": {
                    "bsonType": "string"
                  },
                  "minItems": 1
                },
                "paired": {
                  "bsonType": [
                     "bool",
                     "null"
                  ]
                },
                "library_protocol": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "library_kit": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "organism": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "genome_version": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "sequencing_platform": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "aligner": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "variant_caller": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "date_created": {
                  "bsonType": "date"
                },
                "date_updated": {
                  "bsonType": [
                    "date",
                    "null"
                  ]
                },
                "insert_token": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
              }
            },
            "ownership": {
              "bsonType": "object",
              "required": [
                "inserted_by",
                "edited_by",
                "shared_with"
              ],
              "properties": {
                "inserted_by": {
                  "bsonType": "object",
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "edited_by": {
                  "bsonType": [
                    "object",
                    "null"
                  ],
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "shared_with": {
                  "bsonType": "array",
                  "items": {
                    "bsonType": "object",
                    "required": [
                      "id",
                      "fullname"
                    ],
                    "properties": {
                      "id": {
                        "bsonType": "objectId"
                      },
                      "fullname": {
                        "bsonType": "string"
                      }
                    }
                  }
                }
              }
            },
            "analyses": {
              "bsonType": [
                "array",
                "null"
              ],
              "items": {
                "bsonType": "object",
                "required": [
                  "id",
                  "name",
                  "date_completed"
                ],
                "properties": {
                  "id": {
                    "bsonType": "objectId"
                  },
                  "name": {
                    "bsonType": "string"
                  },
                  "date_completed": {
                    "bsonType": [
                      "date",
                      "null"
                    ]
                  }
                }
              }
            },
            "genome_size": {
              "bsonType": "array",
              "items": {
                "bsonType": "object",
                "required": [
                  "seq",
                  "len"
                ],
                "properties": {
                  "seq": {
                    "bsonType": "string"
                  },
                  "len": {
                    "bsonType": "int"
                  }
                }
              }
            },
            "quality": {
              "bsonType": "object",
              "properties": {
                "reads": {
                  "bsonType": [
                    "object",
                    "null"
                  ]
                },
                "coverage": {
                  "bsonType": [
                    "object",
                    "null"
                  ]
                }
              }
            }
          },
          "additionalProperties": true
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      },
      "validationLevel": "strict"
    }'
    )
}

.defineAnalysesCollection <- function() {
    return(
    '{
      "create": "analyses",
      "validator": {
        "$jsonSchema": {
          "bsonType": "object",
          "required": [
            "_id",
            "name",
            "description",
            "notes",
            "metadata",
            "progress",
            "samples",
            "ownership",
            "stats",
            "toolset",
            "parameters"
          ],
          "properties": {
            "_id": {
              "bsonType": "objectId"
            },
            "name": {
              "bsonType": "string"
            },
            "description": {
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
            },
            "metadata": {
              "bsonType": "object",
              "required": [
                "date_created",
                "date_updated",
                "date_completed",
                "status",
                "organism",
                "genome_version",
                "secondary_protocol",
                "tertiary_protocol",
                "job_id",
                "insert_token"
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
                "date_completed": {
                  "bsonType": [
                    "date",
                    "null"
                  ]
                },
                "status": {
                  "bsonType": "string"
                },
                "fail_reason": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "organism": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "genome_version": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "secondary_protocol": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "tertiary_protocol": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "job_id": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "insert_token": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
              }
            },
            "progress": {
              "bsonType": "object",
              "required": [
                "step",
                "pct",
                "desc"
              ],
              "properties": {
                "step": {
                  "bsonType": [
                    "int"
                  ]
                },
                "pct": {
                  "bsonType": [
                    "int"
                  ]
                },
                "desc": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                }
              }
            },
            "samples": {
              "bsonType": "array",
              "items": {
                "bsonType": "object",
                "required": [
                  "id",
                  "name"
                ],
                "properties": {
                  "id": {
                    "bsonType": "objectId"
                  },
                  "name": {
                    "bsonType": "string"
                  },
                  "tracks": {
                    "bsonType": [
                      "object",
                      "null"
                    ]
                  }
                }
              }
            },
            "ownership": {
              "bsonType": "object",
              "required": [
                "inserted_by",
                "edited_by",
                "shared_with"
              ],
              "properties": {
                "inserted_by": {
                  "bsonType": "object",
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "edited_by": {
                  "bsonType": [
                    "object",
                    "null"
                  ],
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "shared_with": {
                  "bsonType": "array",
                  "items": {
                    "bsonType": "object",
                    "required": [
                      "id",
                      "fullname"
                    ],
                    "properties": {
                      "id": {
                        "bsonType": "objectId"
                      },
                      "fullname": {
                        "bsonType": "string"
                      }
                    }
                  }
                }
              }
            },
            "stats": {
              "bsonType": "object",
              "properties": {
                "variantSpliceScoreRanges": {
                  "bsonType": [
                    "object",
                    "null"
                   ]
                },
                "geneClinDb": {
                  "bsonType": "object",
                  "properties": {
                    "hpo": {
                      "bsonType": "int"
                    },
                    "ctd": {
                      "bsonType": "int"
                    },
                    "omimMorbid": {
                      "bsonType": "int"
                    },
                    "omim": {
                      "bsonType": "int"
                    },
                    "disgenet": {
                      "bsonType": "int"
                    },
                    "cgd": {
                      "bsonType": "int"
                    }
                  }
                },
                "variantStatus": {
                  "bsonType": "object",
                  "properties": {
                    "novel": {
                      "bsonType": "int"
                    },
                    "known": {
                      "bsonType": "int"
                    }
                  }
                },
                "variantLocation": {
                  "bsonType": [
                    "array",
                    "null"
                  ],
                  "items": {
                    "bsonType": [
                      "object",
                      "null"
                    ],
                    "properties": {
                      "type": {
                        "bsonType": [
                          "string",
                          "null"
                        ]
                      },
                      "count": {
                        "bsonType": [
                          "int",
                          "null"
                        ]
                      }
                    }
                  }
                },
                "variantType": {
                  "bsonType": [
                    "array",
                    "null"
                  ],
                  "items": {
                    "bsonType": [
                      "object",
                      "null"
                    ],
                    "properties": {
                      "type": {
                        "bsonType": [
                          "string",
                          "null"
                        ]
                      },
                      "count": {
                        "bsonType": [
                          "int",
                          "null"
                        ]
                      }
                    }
                  }
                },
                "variantInheritance": {
                  "bsonType": [
                    "object",
                    "null"
                  ],
                  "properties": {
                    "dominant": {
                      "bsonType": [
                        "int",
                        "null"
                      ]
                    },
                    "recessive": {
                      "bsonType": [
                        "int",
                        "null"
                      ]
                    },
                    "other": {
                      "bsonType": [
                        "int",
                        "null"
                      ]
                    }
                  }
                },
                "variantClinDb": {
                  "bsonType": [
                    "object",
                    "null"
                  ],
                  "properties": {
                    "disgenet": {
                      "bsonType": "int"
                    },
                    "clinvar_sig": {
                      "bsonType": "int"
                    },
                    "clinvar_onc": {
                      "bsonType": "int"
                    },
                    "civic": {
                      "bsonType": "int"
                    },
                    "oncokb": {
                      "bsonType": "int"
                    }
                  }
                },
                "variantPredictionSummary": {
                  "bsonType": [
                    "object",
                    "null"
                  ]
                },
                "variantPopulation": {
                  "bsonType": [
                    "object",
                    "null"
                  ]
                },
                "variantZygosity": {
                  "bsonType": [
                    "array",
                    "null"
                  ]
                },
                "variantQualityRanges": {
                  "bsonType": "object",
                  "properties": {
                    "minQual": {
                      "bsonType": [
                        "double",
                        "int"
                      ]
                    },
                    "maxQual": {
                      "bsonType": [
                        "double",
                        "int"
                      ]
                    },
                    "minDp": {
                      "bsonType": "int"
                    },
                    "maxDp": {
                      "bsonType": "int"
                    },
                    "minVaf": {
                      "bsonType": [
                        "double",
                        "int"
                      ]
                    },
                    "maxVaf": {
                      "bsonType": [
                        "double",
                        "int"
                      ]
                    }
                  }
                },
                "variantPathogenRanges": {
                  "bsonType": [
                    "object",
                    "null"
                  ]
                },
                "variantClinvar": {
                  "bsonType": "object",
                  "properties": {
                    "sig": {
                      "bsonType": [
                        "array",
                        "null"
                      ],
                      "items": {
                        "bsonType": [
                          "object",
                          "null"
                        ],
                        "properties": {
                          "type": {
                            "bsonType": [
                              "string",
                              "null"
                            ]
                          },
                          "count": {
                            "bsonType": [
                              "int",
                              "null"
                            ]
                          }
                        }
                      }
                    },
                    "onc": {
                      "bsonType": [
                        "array",
                        "null"
                      ],
                      "items": {
                        "bsonType": [
                          "object",
                          "null"
                        ],
                        "properties": {
                          "type": {
                            "bsonType": [
                              "string",
                              "null"
                            ]
                          },
                          "count": {
                            "bsonType": [
                              "int",
                              "null"
                            ]
                          }
                        }
                      }
                    }
                  }
                },
                "geneCgd": {
                  "bsonType": [
                    "object",
                    "null"
                  ],
                  "properties": {
                    "ageGroups": {
                      "bsonType": "array",
                      "items": {
                        "bsonType": [
                          "object",
                          "null"
                        ],
                        "properties": {
                          "type": {
                            "bsonType": [
                              "string",
                              "null"
                            ]
                          },
                          "count": {
                            "bsonType": [
                              "int",
                              "null"
                            ]
                          }
                        }
                      }
                    },
                    "conditions": {
                      "bsonType": "array",
                      "items": {
                        "bsonType": [
                          "object",
                          "null"
                        ],
                        "properties": {
                          "type": {
                            "bsonType": [
                              "string",
                              "null"
                            ]
                          },
                          "count": {
                            "bsonType": [
                              "int",
                              "null"
                            ]
                          }
                        }
                      }
                    },
                    "interventions": {
                      "bsonType": "array",
                      "items": {
                        "bsonType": [
                          "object",
                          "null"
                        ],
                        "properties": {
                          "type": {
                            "bsonType": [
                              "string",
                              "null"
                            ]
                          },
                          "count": {
                            "bsonType": [
                              "int",
                              "null"
                            ]
                          }
                        }
                      }
                    },
                    "manifestation": {
                      "bsonType": "array",
                      "items": {
                        "bsonType": [
                          "object",
                          "null"
                        ],
                        "properties": {
                          "type": {
                            "bsonType": [
                              "string",
                              "null"
                            ]
                          },
                          "count": {
                            "bsonType": [
                              "int",
                              "null"
                            ]
                          }
                        }
                      }
                    }
                  }
                },
                "disease": {
                   "bsonType": [
                      "object",
                      "null"
                   ]
                },
                "defaultFilter": {
                  "bsonType": "object",
                  "properties": {
                    "dp": {
                      "bsonType": "object"
                    },
                    "qual": {
                      "bsonType": "object"
                    },
                    "vaf": {
                      "bsonType": "object"
                    }
                  }
                },
                "effectImpact": {
                  "bsonType": [
                    "array",
                    "null"
                  ],
                  "items": {
                    "bsonType": "object"
                  }
                },
                "presentContigs": {
                  "bsonType": [
                    "array",
                    "null"
                  ],
                  "items": {
                    "bsonType": "string"
                  }
                }
              }
            },
            "toolset": {
              "bsonType": "array",
              "items": {
                "bsonType": [
                  "object",
                  "null"
                ],
                "required": [
                  "name",
                  "version"
                ],
                "properties": {
                  "name": {
                    "bsonType": "string"
                  },
                  "version": {
                    "bsonType": "string"
                  }
                }
              }
            },
            "parameters": {
              "bsonType": [
                "object",
                "null"
              ]
            }
          },
          "additionalProperties": true
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      },
      "validationLevel": "moderate"
    }
    '
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
            "_id"
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
            },
            "sequencing_kit": {
              "bsonType": "array",
              "items": {
                "bsonType": "object",
                "required": [
                  "name",
                  "manufacturer",
                  "product_id"
                ],
                "properties": {
                  "name": {
                    "bsonType": "string"
                  },
                  "manufacturer": {
                    "bsonType": "string"
                  },
                  "product_id": {
                    "bsonType": [
                      "string",
                      "null"
                    ]
                  }
                }
              }
            },
            "variant_type": {
              "bsonType": "array",
              "items": {
                "bsonType": "string"
              }
            },
            "variant_zygosity": {
              "bsonType": "array",
              "items": {
                "bsonType": "string"
              }
            },
            "variant_location": {
              "bsonType": "array",
              "items": {
                "bsonType": "string"
              }
            },
            "variant_effect_snpeff": {
              "bsonType": "array",
              "items": {
                "bsonType": "string"
              }
            },
            "clinvar_sig": {
              "bsonType": "array",
              "items": {
                "bsonType": "string"
              }
            },
            "clinvar_onc": {
              "bsonType": "array",
              "items": {
                "bsonType": "string"
              }
            },
            "so_terms": {
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
      },
      "validationLevel": "moderate"
    }'
    )
}

.defineVariantsCollection <- function() {
    return(
    '{
      "create": "variants" 
    }'
    )
}

#.defineVarstoreVariantsCollection <- function() {
#    return(
#    '{
#      "create": "varstore_variants" 
#    }'
#    )
#}

.defineFiltersCollection <- function() {
    return(
    '{
      "create": "filters",
      "validator": {
        "$jsonSchema": {
          "bsonType": "object",
          "required": [
            "_id",
            "name",
            "metadata",
            "ownership",
            "filter",
            "rule"
          ],
          "properties": {
            "_id": {
              "bsonType": "objectId"
            },
            "name": {
              "bsonType": "string"
            },
            "description": {
              "bsonType": [
                "string",
                "null"
              ]
            },
            "metadata": {
              "bsonType": "object",
              "required": [
                "date_created",
                "date_updated",
                "type"
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
                "type": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                }
              }
            },
            "ownership": {
              "bsonType": "object",
              "required": [
                "inserted_by",
                "edited_by",
                "shared_with"
              ],
              "properties": {
                "inserted_by": {
                  "bsonType": "object",
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "edited_by": {
                  "bsonType": [
                    "object",
                    "null"
                  ],
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "shared_with": {
                  "bsonType": "array",
                  "items": {
                    "bsonType": "object",
                    "required": [
                      "id",
                      "fullname"
                    ],
                    "properties": {
                      "id": {
                        "bsonType": "objectId"
                      },
                      "fullname": {
                        "bsonType": "string"
                      }
                    }
                  }
                }
              }
            },
            "filter": {
              "bsonType": [
                "object",
                "null"
              ]
            },
            "rule": {
              "bsonType": [
                "string",
                "null"
              ]
            }
          },
          "additionalProperties": true
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      },
      "validationLevel": "strict"
    }'
    )
}

.defineSessionsCollection <- function() {
    return(
    '{
      "create": "sessions",
      "validator": {
        "$jsonSchema": {
          "bsonType": "object",
          "required": [
            "_id",
            "name",
            "metadata",
            "ownership",
            "variant_filter",
            "variant_filter_rule",
            "gene_filter",
            "gene_filter_rule",
            "table_page",
            "analysis_id"
          ],
          "properties": {
            "_id": {
              "bsonType": "objectId"
            },
            "name": {
              "bsonType": "string"
            },
            "description": {
              "bsonType": [
                "string",
                "null"
              ]
            },    )
            "metadata": {
              "bsonType": "object",
              "required": [
                "date_created",
                "date_updated",
                "date_loaded"
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
                "date_loaded": {
                  "bsonType": [
                    "date",
                    "null"
                  ]
                }
              }
            },
            "ownership": {
              "bsonType": "object",
              "required": [
                "inserted_by",
                "edited_by",
                "shared_with"
              ],
              "properties": {
                "inserted_by": {
                  "bsonType": "object",
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "edited_by": {
                  "bsonType": [
                    "object",
                    "null"
                  ],
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "shared_with": {
                  "bsonType": "array",
                  "items": {
                    "bsonType": "object",
                    "required": [
                      "id",
                      "fullname"
                    ],
                    "properties": {
                      "id": {
                        "bsonType": "objectId"
                      },
                      "fullname": {
                        "bsonType": "string"
                      }
                    }
                  }
                }
              }
            },
            "variant_filter": {
              "bsonType": [
                "object",
                "null"
              ]
            },
            "variant_filter_rule": {
              "bsonType": [
                "string",
                "null"
              ]
            },
            "gene_filter": {
              "bsonType": [
                "object",
                "null"
              ]
            },
            "gene_filter_rule": {
              "bsonType": [
                "string",
                "null"
              ]
            },
            "cnv_filter": {
              "bsonType": [
                "object",
                "null"
              ]
            },
            "cnv_filter_rule": {
              "bsonType": [
                "string",
                "null"
              ]
            },
            "sv_filter": {
              "bsonType": [
                "object",
                "null"
              ]
            },
            "sv_filter_rule": {
              "bsonType": [
                "string",
                "null"
              ]
            },
            "fusion_filter": {
              "bsonType": [
                "object",
                "null"
              ]
            },
            "fusion_filter_rule": {
              "bsonType": [
                "string",
                "null"
              ]
            },
            "analysis": {
              "bsonType": [
                "object",
                "null"
              ],
              "properties": {
                "id": {
                  "bsonType": [
                    "objectId",
                    "null"
                   ]
                },
                "name": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                }
              }
            },
            "table_page": {
              "bsonType": [
                "object",
                "null"
              ],
              "properties": {
                 "variant": {
                   "bsonType": [
                     "int",
                     "null"
                   ]
                 },
                 "gene": {
                    "bsonType": [
                     "int",
                     "null"
                   ]
                 },
                 "cnv": {
                    "bsonType": [
                     "int",
                     "null"
                   ]
                 },
                 "sv": {
                    "bsonType": [
                     "int",
                     "null"
                   ]
                 },
                 "fusion": {
                    "bsonType": [
                     "int",
                     "null"
                   ]
                 }
              }
            }
          },
          "additionalProperties": true
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      },
      "validationLevel": "strict"
    }'
    )
}

.defineVarstoresCollection <- function() {
    return(
    '{
      "create": "varstores",
      "validator": {
        "$jsonSchema": {
          "bsonType": "object",
          "required": [
            "_id",
            "name",
            "metadata",
            "ownership"
          ],
          "properties": {
            "_id": {
              "bsonType": "objectId"
            },
            "name": {
              "bsonType": "string"
            },
            "description": {
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
            },
            "metadata": {
              "bsonType": "object",
              "required": [
                "date_created",
                "date_updated",
                "organism",
                "genome_version"
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
                "organism": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                },
                "genome_vesion": {
                  "bsonType": [
                    "string",
                    "null"
                  ]
                }
              }
            },
            "ownership": {
              "bsonType": "object",
              "required": [
                "inserted_by",
                "edited_by",
                "shared_with"
              ],
              "properties": {
                "inserted_by": {
                  "bsonType": "object",
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "edited_by": {
                  "bsonType": [
                    "object",
                    "null"
                  ],
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "shared_with": {
                  "bsonType": "array",
                  "items": {
                    "bsonType": "object",
                    "required": [
                      "id",
                      "fullname"
                    ],
                    "properties": {
                      "id": {
                        "bsonType": "objectId"
                      },
                      "fullname": {
                        "bsonType": "string"
                      }
                    }
                  }
                }
              }
            }
          },
          "additionalProperties": true
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      },
      "validationLevel": "strict"
    }'
    )
}

.defineTableviewsCollection <- function() {
    return(
    '{
      "create": "table_views",
      "validator": {
        "$jsonSchema": {
          "bsonType": "object",
          "required": [
            "_id",
            "name",
            "columns",
            "metadata",
            "ownership"
          ],
          "properties": {
            "_id": {
              "bsonType": "objectId"
            },
            "name": {
              "bsonType": "string"
            },
            "description": {
              "bsonType": [
                "string",
                "null"
              ]
            },
            "columns": {
              "bsonType": "array",
              "items": {
                "bsonType": "string"
              }
            },
            "metadata": {
              "bsonType": "object",
              "required": [
                "type",
                "date_created",
                "date_updated"
              ],
              "properties": {
                "type": {
                  "bsonType": "string"
                },
                "date_created": {
                  "bsonType": "date"
                },
                "date_updated": {
                  "bsonType": [
                    "date",
                    "null"
                  ]
                }
              }
            },
            "ownership": {
              "bsonType": "object",
              "required": [
                "inserted_by",
                "edited_by",
                "shared_with"
              ],
              "properties": {
                "inserted_by": {
                  "bsonType": "object",
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "edited_by": {
                  "bsonType": [
                    "object",
                    "null"
                  ],
                  "required": [
                    "id",
                    "fullname"
                  ],
                  "properties": {
                    "id": {
                      "bsonType": "objectId"
                    },
                    "fullname": {
                      "bsonType": "string"
                    }
                  }
                },
                "shared_with": {
                  "bsonType": "array",
                  "items": {
                    "bsonType": "object",
                    "required": [
                      "id",
                      "fullname"
                    ],
                    "properties": {
                      "id": {
                        "bsonType": "objectId"
                      },
                      "fullname": {
                        "bsonType": "string"
                      }
                    }
                  }
                }
              }
            }
          },
          "additionalProperties": true
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      },
      "validationLevel": "strict"
    }'
    )
}

# Regarding backend databases, this should be added to the build scripts
#db.disgenet_gene.createIndex({
#   "gene_name": 1
#})


#~ "disease": {
#~   "bsonType": "object",
#~   "properties": {
#~  "classes": {
#~    "bsonType": [
#~      "array",
#~      "null"
#~    ],
#~    "items": {
#~      "bsonType": [
#~        "object",
#~        "null"
#~      ],
#~      "properties": {
#~        "type": {
#~          "bsonType": [
#~            "string",
#~            "null"
#~          ]
#~        },
#~        "count": {
#~          "bsonType": [
#~            "int",
#~            "null"
#~          ]
#~        }
#~      }
#~    }
#~  },
#~  "diseases": {
#~    "bsonType": [
#~      "int",
#~      "null"
#~    ]
#~  }
#~   }
#~ },
